%%%----------------------------------------------------------------------
%%% File    : mod_grafite.erl
%%% Author  : Thiago Rocha Camargo
%%% Purpose : Calculates and gathers statistics actively
%%% Created :
%%% Id      : $Id: mod_grafite.erl 0000 2016-07-11 16:42:30Z xmppjingle $
%%%----------------------------------------------------------------------

%%%% Definitions

-module(mod_grafite).
-author('rochacamargothiago@gmail.com').
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(HOOKS, [offline_message_hook,
                sm_register_connection_hook, sm_remove_connection_hook,
                user_send_packet, user_receive_packet,
                s2s_send_packet, s2s_receive_packet,
                remove_user, register_user, component_register]).

-export([start/2, stop/1, mod_opt_type/1,
   depends/2, udp_loop_start/1]).

-export([offline_message_hook/3,
         sm_register_connection_hook/3, sm_remove_connection_hook/3,
         user_send_packet/4, user_receive_packet/5,
         s2s_send_packet/3, s2s_receive_packet/3,
         remove_user/2, register_user/2, component_register/1]).

-record(state, {socket, host, port}).

-define(PROCNAME, ejabberd_mod_grafite).

%%====================================================================
%% API
%%====================================================================

start(Host, Opts) ->
    [ejabberd_hooks:add(Hook, Host, ?MODULE, Hook, 20)
     || Hook <- ?HOOKS],
     StatsDHost = gen_mod:get_opt(statsdhost, Opts, fun(X) -> X end, "localhost"),
     StatsDPort = gen_mod:get_opt(statsdport, Opts, fun(X) -> X end, 5002),
     register(?PROCNAME, spawn(?MODULE, udp_loop_start, [#state{host = getaddrs(StatsDHost), port = StatsDPort}])).

stop(Host) ->
    [ejabberd_hooks:delete(Hook, Host, ?MODULE, Hook, 20)
     || Hook <- ?HOOKS].

depends(_Host, _Opts) ->
    [].

%%====================================================================
%% Hooks handlers
%%====================================================================

offline_message_hook(_From, #jid{lserver=LServer}, _Packet) ->
    push(LServer, offline_message).

sm_register_connection_hook(_SID, #jid{lserver=LServer}, _Info) ->
    push(LServer, sm_register_connection).
sm_remove_connection_hook(_SID, #jid{lserver=LServer}, _Info) ->
    push(LServer, sm_remove_connection).

user_send_packet(Packet, _C2SState, #jid{lserver=LServer}, _To) ->
    push(LServer, user_send_packet),
    Packet.
user_receive_packet(Packet, _C2SState, _JID, _From, #jid{lserver=LServer}) ->
    push(LServer, user_receive_packet),
    Packet.

s2s_send_packet(#jid{lserver=LServer}, _To, _Packet) ->
    push(LServer, s2s_send_packet).
s2s_receive_packet(_From, #jid{lserver=LServer}, _Packet) ->
    push(LServer, s2s_receive_packet).

remove_user(_User, Server) ->
    push(jid:nameprep(Server), remove_user).
register_user(_User, Server) ->
    push(jid:nameprep(Server), register_user).

component_register(Host) ->
    push(Host, component_register).

%%====================================================================
%% metrics push handler
%%====================================================================

push(Host, Probe) ->
    Payload = encode_metrics(Host, Probe),
    whereis(?PROCNAME) ! {send, Payload}.

encode_metrics(Host, Probe) ->
    [_, NodeId] = str:tokens(jlib:atom_to_binary(node()), <<"@">>),
    [Node | _] = str:tokens(NodeId, <<".">>),
    BaseId = <<Host/binary, "/", Node/binary, ".">>,
    DateTime = erlang:universaltime(),
    UnixTime = calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200,
    TS = integer_to_binary(UnixTime),
    Data = case Probe of
    {Key, Val} ->
        BVal = integer_to_binary(Val),
        <<BaseId/binary, (jlib:atom_to_binary(Key))/binary,
          ":g/", TS/binary, ":", BVal/binary>>;          
    Key ->
        <<BaseId/binary, (jlib:atom_to_binary(Key))/binary,
          ":c/", TS/binary, ":1">>
    end,
    ?INFO_MSG("Stats: ~p ~p ~n", [Data, encode(gauge, Key, 1, undefined)]),
    Data.

%%====================================================================
%% Grafite/StatsD
%%====================================================================

encode(gauge, Key, Value, _SampleRate) ->
    [Key, ":", format_value(Value), "|g"].

format_value(Value) when is_integer(Value) ->
    integer_to_list(Value);
format_value(Value) when is_float(Value) ->
    float_to_list(Value, [{decimals, 2}]).

%%====================================================================
%% UDP Utils
%%====================================================================

udp_loop_start(#state{}=S) ->
  LocalPort = 44444,
  case gen_udp:open(LocalPort) of
    {ok, Socket} ->
      ?INFO_MSG("UDP Stats Socket Open: [~p]~n", [LocalPort]),
      udp_loop(S#state{socket = Socket});
    _ ->
      ?INFO_MSG("Could not start UDP Socket [~p]~n", [LocalPort])
  end.

udp_loop(#state{} = S) ->
  receive 
    {send, Packet} ->
        send_udp(Packet, S),
        udp_loop(S);
    _ ->
      ok
  end.

send_udp(Payload, #state{socket = Socket, host = Host, port = Port} = State) ->
    gen_udp:send(Socket, Host, Port, Payload),
    {ok, State}.

getaddrs({_, _, _, _} = Address) ->
    {ok, Address};
getaddrs(Hostname) when is_binary(Hostname) ->
    getaddrs(binary_to_list(Hostname));
getaddrs(Hostname) ->
    case inet:getaddrs(Hostname, inet) of
        {ok, Addrs} ->
            {ok, random_element(Addrs)};
        {error, Reason} ->
            ?ERROR_MSG("getaddrs error: ~p~n", [Reason]),
            {error, Reason}
    end.

random_element([Element]) ->
    Element;
random_element([_|_] = List) ->
    T = list_to_tuple(List),
    Index = random(tuple_size(T)),
    element(Index, T).

random(N) ->
    erlang:phash2({self(), timestamp()}, N) + 1.

timestamp() ->
    os:timestamp().

mod_opt_type(statsdhost) -> fun(X) -> X end;
mod_opt_type(statsdport) -> fun(X) when is_integer(X) -> X end;
mod_opt_type(_) ->
    [statsdhost, statsdport].
