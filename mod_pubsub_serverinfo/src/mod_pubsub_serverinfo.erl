%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_serverinfo.erl
%%% Author  : Guus der Kinderen <guus.der.kinderen@gmail.com>
%%% Purpose : Exposes server information over Pub/Sub
%%% Created : 26 Dec 2023 by Guus der Kinderen <guus.der.kinderen@gmail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2023   ProcessOne
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

-module(mod_pubsub_serverinfo).
-author('guus.der.kinderen@gmail.com').
-behaviour(gen_mod).
-behaviour(gen_server).

-include("pubsub_serverinfo_codec.hrl").
-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

%% gen_mod callbacks.
-export([start/2, stop/1, depends/2, mod_options/1, get_local_features/5, mod_doc/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).
-export([in_auth_result/3, out_auth_result/2, get_info/5]).

-define(NS_URN_SERVERINFO, <<"urn:xmpp:serverinfo:0">>).

-record(state, {host, pubsub_host, node, monitors = #{}, timer = undefined}).

start(Host, Opts) ->
    xmpp:register_codec(pubsub_serverinfo_codec),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, get_local_features, 50),
    ejabberd_hooks:add(disco_info, Host, ?MODULE, get_info, 50),
    ejabberd_hooks:add(s2s_out_auth_result, Host, ?MODULE, out_auth_result, 50),
    ejabberd_hooks:add(s2s_in_auth_result, Host, ?MODULE, in_auth_result, 50),
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, get_local_features, 50),
    ejabberd_hooks:delete(disco_info, Host, ?MODULE, get_info, 50),
    ejabberd_hooks:delete(s2s_out_auth_result, Host, ?MODULE, out_auth_result, 50),
    ejabberd_hooks:delete(s2s_in_auth_result, Host, ?MODULE, in_auth_result, 50),
    gen_mod:stop_child(?MODULE, Host).

init([Host, _Opts]) ->
    TRef = timer:send_interval(timer:minutes(5), self(), update_pubsub),
    Monitors = init_monitors(Host),
    %% [FIXME] pubsub_host shouldn't just be hardcoded, there should be a config option to reflect
    %% the `hosts` option of mod_pubsub's configuration
    State = #state{host = Host, pubsub_host = <<"pubsub.", Host/binary>>, node = <<"serverinfo">>, timer = TRef, monitors = Monitors},
    self() ! update_pubsub,
    {ok, State}.

init_monitors(Host) ->
    lists:foldl(
      fun(Domain, Monitors) ->
              RefIn = make_ref(), % just dummies
              RefOut = make_ref(),
              maps:merge(#{RefIn => {incoming, {Host, Domain, true}},
                           RefOut => {outgoing, {Host, Domain, true}}},
                         Monitors)
      end,
      #{},
      ejabberd_option:hosts() -- [Host]).

handle_cast({Event, Host, LServer, RServer, Pid}, #state{monitors = Mons} = State)
  when Event == register_in; Event == register_out ->
    Ref = monitor(process, Pid),
    HasSupport = check_if_remote_has_support(Host, LServer, RServer, Mons),
    NewMons = maps:put(Ref, {event_to_dir(Event), {LServer, RServer, HasSupport}}, Mons),
    {noreply, State#state{monitors = NewMons}};
handle_cast(_, State) ->
    {noreply, State}.

event_to_dir(register_in) -> incoming;
event_to_dir(register_out) -> outgoing.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_info({iq_reply, IQReply, {LServer, RServer}},
	    #state{monitors = Mons} = State) ->
    case IQReply of
	#iq{type = result, sub_els = [El]} ->
	    case xmpp:decode(El) of
		#disco_info{features = Features} ->
		    case lists:member(?NS_URN_SERVERINFO, Features) of
			true ->
			    NewMons = maps:fold(fun(Ref, {Dir, {LServer0, RServer0, _}}, Acc)
                                    when LServer == LServer0, RServer == RServer0 ->
                                      maps:put(Ref, {Dir, {LServer, RServer, true}}, Acc);
                                 (Ref, Other, Acc) ->
                                      maps:put(Ref, Other, Acc)
                              end,
                              #{},
                              Mons),
			    {noreply, State#state{monitors = NewMons}};
			_ ->
			    {noreply, State}
		    end;
		_ ->
		    {noreply, State}
	    end;
	_ ->
	    {noreply, State}
    end;
handle_info(update_pubsub, State) ->
    update_pubsub(State),
    {noreply, State};
handle_info({'DOWN', Mon, process, _Pid, _Info}, #state{monitors = Mons} = State) ->
    {noreply, State#state{monitors = maps:remove(Mon, Mons)}};
handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, #state{monitors = Mons, timer = Timer}) ->
    case is_reference(Timer) of
	true ->
	    case erlang:cancel_timer(Timer) of
		false ->
		    receive {timeout, Timer, _} -> ok
		    after 0 -> ok
		    end;
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    maps:fold(
	fun(Mon, _, _) ->
	    demonitor(Mon)
	end, ok, Mons).

depends(_Host, _Opts) ->
    [{mod_pubsub, hard}].

mod_options(_Host) ->
    [].

mod_doc() -> #{}.

in_auth_result(#{server_host := Host, server := LServer, remote_server := RServer} = State, true, _Server) ->
    gen_server:cast(gen_mod:get_module_proc(Host, ?MODULE), {register_in, Host, LServer, RServer, self()}),
    State;
in_auth_result(State, _, _) ->
    State.

out_auth_result(#{server_host := Host, server := LServer, remote_server := RServer} = State, true) ->
    gen_server:cast(gen_mod:get_module_proc(Host, ?MODULE), {register_out, Host, LServer, RServer, self()}),
    State;
out_auth_result(State, _) ->
    State.

check_if_remote_has_support(Host, LServer, RServer, Mons) ->
    maybe_send_disco_info(has_support(LServer, RServer, Mons), Host, LServer, RServer).

has_support(LServer, RServer, Mons) ->
   maps:size(
     maps:filter(
       fun(_Ref, {_Dir, {LServer0, RServer0, HasSupport}})
          when LServer0 == LServer, RServer0 == RServer -> HasSupport;
          (_Ref, _Other) -> false
       end,
       Mons)) =/= 0.

maybe_send_disco_info(true, _Host, _LServer, _RServer) -> true;
maybe_send_disco_info(false, Host, LServer, RServer) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    IQ = #iq{type = get, from = jid:make(LServer),
	     to = jid:make(RServer), sub_els = [#disco_info{}]},
    ejabberd_router:route_iq(IQ, {LServer, RServer}, Proc),
    false.

update_pubsub(#state{host = Host, pubsub_host = PubsubHost, node = Node, monitors = Mons}) ->
    Map = maps:fold(
            fun(_, {Dir, {MyDomain, Target, HasSupport}}, Acc) ->
                    maps:update_with(MyDomain,
                                     fun(Acc2) ->
                                             maps:update_with(Target,
                                                              fun({Types, _}) -> {Types#{Dir => true}, HasSupport} end,
                                                              {#{Dir => true}, HasSupport}, Acc2)
                                     end, #{Target => {#{Dir => true}, HasSupport}}, Acc)
            end, #{}, Mons),
    Domains = maps:fold(
                fun(MyDomain, Targets, Acc) ->
                        Remote = maps:fold(
                                   fun(Remote, {Types, true}, Acc2) ->
                                           [#pubsub_serverinfo_remote_domain{name = Remote, type = maps:keys(Types)} | Acc2];
                                      (_HiddenRemote, {Types, false}, Acc2) ->
                                           [#pubsub_serverinfo_remote_domain{type = maps:keys(Types)} | Acc2]
                                   end, [], Targets),
                        [#pubsub_serverinfo_domain{name = MyDomain, remote_domain = Remote} | Acc]
                end, [], Map),

    PubOpts = [{persist_items, true}, {max_items, 1}, {access_model, open}],
    ?DEBUG("Publishing serverinfo pubsub item on ~s: ~p", [PubsubHost, Domains]),
    mod_pubsub:publish_item(
      PubsubHost, Host, Node, jid:make(Host),
      <<"current">>, [xmpp:encode(#pubsub_serverinfo{domain = Domains})], PubOpts, all).

get_local_features({error, _} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_local_features(Acc, _From, _To, Node, _Lang) when Node == <<>> ->
    case Acc of
	{result, Features} ->
	    {result, [?NS_URN_SERVERINFO | Features]};
	empty -> {result, [?NS_URN_SERVERINFO]}
    end;
get_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_info(Acc, Host, Mod, Node, Lang) when (Mod == undefined orelse Mod == mod_disco), Node == <<"">> ->
    case mod_disco:get_info(Acc, Host, Mod, Node, Lang) of
	[#xdata{fields = Fields} = XD | Rest] ->
	    NodeField = #xdata_field{var = <<"serverinfo-pubsub-node">>,
                               %% [FIXME] don't hardcode pubsub host (see above)
		                           values = [<<"xmpp:pubsub.", Host/binary, "?;node=serverinfo">>]},
	    {stop, [XD#xdata{fields = Fields ++ [NodeField]} | Rest]};
	_ ->
	    Acc
    end;
get_info(Acc, Host, Mod, Node, _Lang) when Node == <<"">>, is_atom(Mod) ->
    [#xdata{type = result,
	fields = [
	    #xdata_field{type = hidden,
		var = <<"FORM_TYPE">>,
		values = [?NS_SERVERINFO]},
	    #xdata_field{var = <<"serverinfo-pubsub-node">>,
                   %% [FIXME] don't hardcode pubsub host (see above)
                   values = [<<"xmpp:pubsub.", Host/binary, "?;node=serverinfo">>]}]} | Acc];
get_info(Acc, _Host, _Mod, _Node, _Lang) ->
    Acc.
