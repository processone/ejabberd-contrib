%%%----------------------------------------------------------------------
%%% File    : mod_deny_omemo.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Prevent OMEMO sessions from being established
%%% Created : 18 Mar 2018 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2018   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_deny_omemo).
-author('holger@zedat.fu-berlin.de').
-behavior(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, mod_opt_type/1, depends/2, mod_options/1]).

%% ejabberd_hooks callbacks.
-export([user_receive_packet/1, user_send_packet/1]).

-include("logger.hrl").
-include("xmpp.hrl").

-define(NS_AXOLOTL, "eu.siacs.conversations.axolotl").
-define(DEVICELIST_NODE, ?NS_AXOLOTL ".devicelist").

-type c2s_state() :: ejabberd_c2s:state().
-type hook_result() :: {stanza() | drop, c2s_state()} |
		       {stop, {drop, c2s_state()}}.

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, _Opts) ->
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send_packet, 50),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       user_receive_packet, 50).

-spec stop(binary()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send_packet, 50),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  user_receive_packet, 50).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(_Host, _NewOpts, _OldOpts) ->
    ok.

-spec mod_opt_type(atom()) -> fun((term()) -> term()).
mod_opt_type(access) ->
    fun acl:access_rules_validator/1.

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(_Host) ->
    [{access, omemo}].

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [{mod_pubsub, hard}].

%%--------------------------------------------------------------------
%% ejabberd_hooks callbacks.
%%--------------------------------------------------------------------
-spec user_send_packet({stanza() | drop, c2s_state()}) -> hook_result().
user_send_packet({#message{}, _C2SState} = Acc) ->
    maybe_reject_msg(Acc);
user_send_packet({#iq{type = set,
		      from = #jid{luser = LUser, lserver = LServer},
		      to = #jid{luser = LUser, lserver = LServer,
				lresource = <<>>},
		      sub_els = [SubEl]}, _C2SState} = Acc) ->
    try xmpp:decode(SubEl) of
	#pubsub{publish = Publish} ->
	    maybe_reject_iq(Publish, Acc);
	_ ->
	    Acc
    catch _:{xmpp_codec, _Reason} ->
	    Acc
    end;
user_send_packet({#iq{type = get,
		      from = #jid{luser = FromU, lserver = FromS},
		      to = #jid{luser = ToU, lserver = ToS,
				lresource = <<>>},
		      sub_els = [SubEl]}, _C2SState} = Acc)
  when FromU /= ToU;
       FromS /= ToS ->
    try xmpp:decode(SubEl) of
	#pubsub{items = Items} ->
	    maybe_reject_iq(Items, Acc);
	_ ->
	    Acc
    catch _:{xmpp_codec, _Reason} ->
	    Acc
    end;
user_send_packet(Acc) ->
    Acc.

-spec user_receive_packet({stanza() | drop, c2s_state()}) -> hook_result().
user_receive_packet({#message{} = Msg,
		     #{lserver := LServer, jid := JID} = C2SState} = Acc) ->
    case xmpp:get_subtag(Msg, #ps_event{}) of
	#ps_event{items = Items} ->
	    Access = gen_mod:get_module_opt(LServer, ?MODULE, access),
	    case acl:match_rule(LServer, Access, JID) of
		allow ->
		    Acc;
		deny ->
		    case find_omemo_nodes(Items) of
			[] ->
			    Acc;
			_Nodes ->
			    ?DEBUG("Dropping devicelist update sent to ~s",
				   [jid:encode(JID)]),
			    {stop, {drop, C2SState}}
		    end
	    end;
	_ ->
	    maybe_reject_msg(Acc)
    end;
user_receive_packet(Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec maybe_reject_msg({message(), c2s_state()})
      -> {message(), c2s_state()} | {stop, {drop, c2s_state()}}.
maybe_reject_msg({#message{lang = Lang} = Msg,
		  #{lserver := LServer, jid := JID} = C2SState} = Acc) ->
    Access = gen_mod:get_module_opt(LServer, ?MODULE, access),
    case acl:match_rule(LServer, Access, JID) of
	allow ->
	    Acc;
	deny ->
	    case is_omemo_msg(Msg) of
		true ->
		    ?DEBUG("Rejecting message from ~s", [jid:encode(JID)]),
		    bounce_error(Msg, Lang),
		    {stop, {drop, C2SState}};
		false ->
		    Acc
	    end
    end.

-spec maybe_reject_iq(ps_publish() | ps_items(), {iq(), c2s_state()})
      -> {iq(), c2s_state()} | {stop, {drop, c2s_state()}}.
maybe_reject_iq(El, {#iq{type = Type, lang = Lang} = IQ,
		     #{lserver := LServer, jid := JID} = C2SState} = Acc) ->
    Access = gen_mod:get_module_opt(LServer, ?MODULE, access),
    case acl:match_rule(LServer, Access, JID) of
	allow ->
	    Acc;
	deny ->
	    case find_omemo_nodes(El) of
		[] ->
		    Acc;
		Nodes ->
		    ?DEBUG("Rejecting IQ ~s of ~s", [Type, jid:encode(JID)]),
		    case Type of
			set -> delete_nodes(JID, Nodes);
			get -> ok
		    end,
		    bounce_error(IQ, Lang),
		    {stop, {drop, C2SState}}
	    end
    end.

-spec find_omemo_nodes(ps_publish() | ps_items()) -> [binary()].
find_omemo_nodes(#ps_items{node = Node, items = Items}) ->
    find_omemo_nodes(Node, Items);
find_omemo_nodes(#ps_publish{node = Node, items = Items}) ->
    find_omemo_nodes(Node, Items);
find_omemo_nodes(_) ->
    [].

-spec find_omemo_nodes(binary(), ps_items()) -> [binary()].
find_omemo_nodes(<<?DEVICELIST_NODE>> = Node, [Item]) ->
    [Node | find_bundle_nodes(Item)];
find_omemo_nodes(<<?DEVICELIST_NODE>> = Node, []) ->
    [Node];
find_omemo_nodes(_Node, _Item) ->
    [].

-spec find_bundle_nodes(ps_item()) -> [binary()].
find_bundle_nodes(#ps_item{sub_els = [#xmlel{name = <<"list">>,
					     attrs = [{<<"xmlns">>,
						       <<?NS_AXOLOTL>>}],
					     children = Devices}]}) ->
    [<<?NS_AXOLOTL, ".bundles:", ID/binary>>
     || #xmlel{name = <<"device">>, attrs = [{<<"id">>, ID}]} <- Devices];
find_bundle_nodes(_Item) ->
    [].

-spec delete_nodes(jid(), [binary()]) -> ok.
delete_nodes(JID, Nodes) ->
    LJID = jid:remove_resource(jid:tolower(JID)),
    ?DEBUG("Removing the following nodes of ~s: ~p",
	   [jid:encode(LJID), Nodes]),
    lists:foreach(fun(Node) ->
			  mod_pubsub:delete_node(LJID, Node, JID)
		  end, Nodes).

-spec is_omemo_msg(message()) -> boolean().
is_omemo_msg(#message{type = error}) ->
    false;
is_omemo_msg(#message{sub_els = SubEls}) ->
    case lists:keyfind(<<"encrypted">>, #xmlel.name, SubEls) of
	#xmlel{attrs = Attrs} ->
	    lists:member({<<"xmlns">>, <<?NS_AXOLOTL>>}, Attrs);
	false ->
	    false
    end.

-spec bounce_error(stanza(), binary()) -> ok.
bounce_error(Pkt, Lang) ->
    Txt = <<"OMEMO is disabled">>,
    Err = xmpp:err_policy_violation(Txt, Lang),
    ejabberd_router:route_error(Pkt, Err).
