%%%----------------------------------------------------------------------
%%% File    : mod_push_offline.erl
%%% Author  : Mujtaba Roohani <mujtaba.roohani@gamil.com>
%%% Purpose : Send offline messages to a component
%%% 
%%% Copyright (C) 2022 Mujtaba Roohani
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <https://www.gnu.org/licenses/>
%%%----------------------------------------------------------------------

-module(mod_push_offline).
-author('mujtaba.roohani@gmail.com').
-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, mod_opt_type/1, mod_options/1, depends/2]).
-export([mod_doc/0]).
%% ejabberd_hooks callbacks.
-export([offline_message/1]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, _) ->
    register_hooks(Host).

-spec stop(binary()) -> ok.
stop(Host) ->
    unregister_hooks(Host).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(_, _, _) ->
    ok.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(host) ->
    econf:host().

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(Host) ->
    [{host, <<"push.", Host/binary>>}].

mod_doc() ->
    #{desc =>
          ?T("This is an ejabberd module that sends all messages sent to an unavailable entity to the" 
            "specified component. It is a small modification of `mod_push`, customized"
            "for development of advanced push notification services."),
      opts =>
          [{host,
            #{value => "Host",
              desc =>
                  ?T("This option defines the host to receive offline messages from the service. "
                     "If the 'host' option is not specified, the host will be "
                     "the hostname of the virtual host with the prefix \"push.\". ")}}]}.

%%--------------------------------------------------------------------
%% Register/unregister hooks.
%%--------------------------------------------------------------------
-spec register_hooks(binary()) -> ok.
register_hooks(Host) ->
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE,
		       offline_message, 55).

-spec unregister_hooks(binary()) -> ok.
unregister_hooks(Host) ->
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE,
			  offline_message, 55).

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------
-spec offline_message({any(), message()}) -> {any(), message()}.
offline_message({offlined,
		 #message{to = To} = Pkt} = Acc) ->
    ?DEBUG("Notifying ~ts of offline message", [jid:encode(To)]),
	notify(To, Pkt),
    Acc;
offline_message(Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% Generate push notifications.
%%--------------------------------------------------------------------
-spec notify(jid(), xmpp_element() | xmlel() | none) -> ok.
notify(#jid{lserver = LServer} = To, Pkt) ->
    UnWrappedPkt = unwrap_message(Pkt),
	DelayedPkt = add_delay_info(UnWrappedPkt, LServer),
	Id = p1_rand:get_string(),
	PushServer = mod_push_offline_opt:host(LServer),
	WrappedPacket = wrap(DelayedPkt, <<"urn:xmpp:push:nodes:messages">>, Id),
	ejabberd_router:route(xmpp:set_from_to(WrappedPacket, To, jid:make(PushServer))).

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec unwrap_message(Stanza) -> Stanza when Stanza :: stanza() | none.
unwrap_message(#message{meta = #{carbon_copy := true}} = Msg) ->
    misc:unwrap_carbon(Msg);
unwrap_message(#message{type = normal} = Msg) ->
    case misc:unwrap_mucsub_message(Msg) of
	#message{} = InnerMsg ->
	    InnerMsg;
	false ->
	    Msg
    end;
unwrap_message(Stanza) ->
    Stanza.

-spec wrap(stanza(), binary(), binary()) -> message().
wrap(Packet, Node, Id) ->
    #message{
	id = Id,
	sub_els = [#ps_event{
	    items = #ps_items{
		node = Node,
		items = [#ps_item{
		    id = Id,
		    sub_els = [Packet]}]}}]}.

-spec add_delay_info(message(), binary()) -> message().
add_delay_info(Packet, LServer) ->
    Packet1 = xmpp:put_meta(Packet, from_offline, true),
    misc:add_delay_info(Packet1, jid:make(LServer), erlang:timestamp(),
			<<"Offline storage">>).