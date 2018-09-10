-module(mod_cobrowser).

-behaviour(gen_mod).

%% Required by ?DEBUG macros
-include("logger.hrl").
-include("xmpp.hrl").

%% gen_mod API callbacks
-export([start/2, stop/1, on_user_send_packet/1, on_disconnect/3, send_availability/3, getenv/2]).

start(Host, _Opts) ->
    ?INFO_MSG("mod_cobrowser starting", []),
    inets:start(),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 50),
    ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE, on_disconnect, 50),
    ?INFO_MSG("mod_cobrowser hooks attached", []),
    ok.

stop(Host) ->
    ?INFO_MSG("mod_cobrowser stopping", []),
    
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 50),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host, ?MODULE, on_disconnect, 50),
    ok.

-spec on_user_send_packet({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.

on_user_send_packet({#presence{
                        from = #jid{lresource = <<"">>} = From,
                        show = Show,
                        type = unavailable = Type} = Pkt, State} ) ->

      Jid = binary_to_list(jlib:jid_to_string(From)),
      BareJid = string:sub_string(Jid,1,string:str(Jid,"/")-1),
      send_availability(BareJid, Type, Show),
    {Pkt, State};
on_user_send_packet({#presence{
                        from = From,
                        show = Show,
                        type = available = Type} = Pkt, State} ) ->

      Jid = binary_to_list(jlib:jid_to_string(From)),
      BareJid = string:sub_string(Jid,1,string:str(Jid,"/")-1),
      send_availability(BareJid, Type, Show),
    {Pkt, State};
on_user_send_packet(Acc) ->
    Acc.

on_disconnect(Sid, Jid, Info ) ->
    StrJid = binary_to_list(jlib:jid_to_string(Jid)),
    BareJid = string:sub_string(StrJid,1,string:str(StrJid,"/")-1),
    ?DEBUG("(mod_cobrowser)onDisconnect: ~p, ~p, ~p", [ Sid, BareJid, Info]),
    send_availability(BareJid, unavailable, undefined),

    ok.

send_availability(Jid, Type, Show) ->
      APIHost = getenv("NGINX_INTERNAL_SERVICE_HOST", "nginx-internal.default.svc.cluster.local"),
      APIEndpoint = "http://" ++ APIHost ++ "/api/app.php/internal/user-presence.json",
      ShowString = lists:flatten(io_lib:format("~p", [ Show])),
      TypeString = lists:flatten(io_lib:format("~p", [ Type])),
      ?DEBUG("sending packet: ~p type: ~p show: ~p api: ~p", [ Jid, Type, Show, APIEndpoint]),
      URL = "jid=" ++ Jid ++ "&type=" ++ TypeString ++ "&show=" ++ ShowString,
      R = httpc:request(post, {
          APIEndpoint,
          [],
          "application/x-www-form-urlencoded",
          URL}, [], []),
      {ok, {{"HTTP/1.1", ReturnCode, _}, _, _}} = R,
      ?DEBUG("api request made with result -> ~p ", [ ReturnCode]),
      ReturnCode.

getenv(VarName, DefaultValue) ->
    case os:getenv(VarName) of
        false ->
           DefaultValue;
        Value ->
            Value
    end.
