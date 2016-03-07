%%%----------------------------------------------------------------------
%%% File    : mod_post_log.erl
%%% Author  : Tim Stewart <tim@stoo.org>
%%% Purpose : POST user messages to server via HTTP
%%% Created : 02 Aug 2014 by Tim Stewart <tim@stoo.org>
%%%
%%% Based on mod_service_log.erl
%%%----------------------------------------------------------------------

-module(mod_post_log).
-author('tim@stoo.org').

-behaviour(gen_mod).

-export([start/2,
         stop/1,
         log_user_send/4,
         post_result/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, _Opts) ->
    ok = case inets:start() of
             {error, {already_started, inets}} ->
                 ok;
             ok ->
                 ok
         end,
    ejabberd_hooks:add(user_send_packet, Host,
                       ?MODULE, log_user_send, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host,
                          ?MODULE, log_user_send, 50),
    ok.

log_user_send(Packet, _C2SState, From, To) ->
    ok = log_packet(From, To, Packet),
    Packet.

log_packet(From, To, #xmlel{name = <<"message">>} = Packet) ->
    ok = log_message(From, To, Packet);

log_packet(_From, _To, _Packet) ->
    ok.

log_message(From, To, #xmlel{attrs = Attrs} = Packet) ->
    Type = lists:keyfind(<<"type">>, 1, Attrs),
    log_message_filter(Type, From, To, Packet).

log_message_filter({<<"type">>, Type}, From, To, Packet)
  when Type =:= <<"chat">>;
       Type =:= <<"groupchat">> ->
    log_chat(From, To, Packet);
log_message_filter(_Other, _From, _To, _Packet) ->
    ok.

log_chat(From, To, #xmlel{children = Els} = Packet) ->
    case get_body(Els) of
        no_body ->
            ok;
        {ok, _Body} ->
            log_chat_with_body(From, To, Packet)
    end.

log_chat_with_body(From, To, Packet) ->
    post_xml(From, To, fxml:element_to_binary(Packet)).

post_xml(From, To, Xml) ->
    Ts = to_iso_8601_date(os:timestamp()),

    Body = Xml,

    Url = get_opt(url),
    TsHeader = get_opt(ts_header, "X-Message-Timestamp"),
    FromHeader = get_opt(from_header, "X-Message-From"),
    ToHeader = get_opt(to_header, "X-Message-To"),
    Headers = [ {TsHeader,   Ts},
                {FromHeader, format_jid(From)},
                {ToHeader,   format_jid(To)}
                | get_opt(headers, []) ],
    ContentType = get_opt(content_type, "text/xml"),
    HttpOptions = get_opt(http_options, []),
    ReqOptions = get_opt(req_options, []),

    {ok, _ReqId} = httpc:request(post,
                                 {Url, Headers, ContentType, Body},
                                 HttpOptions,
                                 [ {sync, false},
                                   {receiver, {?MODULE, post_result, []}}
                                   | ReqOptions ]),
    ok.

post_result({_ReqId, {error, Reason}}) ->
    report_error([ {error, Reason } ]);
post_result({_ReqId, Result}) ->
    {StatusLine, Headers, Body} = Result,
    {_HttpVersion, StatusCode, ReasonPhrase} = StatusLine,
    if StatusCode < 200;
       StatusCode > 299 ->
            ok = report_error([ {status_code,   StatusCode},
                                {reason_phrase, ReasonPhrase},
                                {headers,       Headers},
                                {body,          Body} ]),
            ok;
       true ->
            ok
    end.

get_body(Els) ->
    XmlEls = [ El || El <- Els, is_record(El, xmlel) ],
    case lists:keyfind(<<"body">>, #xmlel.name, XmlEls) of
        false ->
            no_body;
        #xmlel{children = InnerEls} ->
            case lists:keyfind(xmlcdata, 1, InnerEls) of
                false ->
                    no_body;
                {xmlcdata, Body} ->
                    {ok, Body}
            end
    end.

get_opt(Opt) ->
    get_opt(Opt, undefined).

get_opt(Opt, Default) ->
    F = fun(Val) when is_binary(Val) -> binary_to_list(Val);
           (Val)                     -> Val
        end,
    gen_mod:get_module_opt(global, ?MODULE, Opt, F, Default).

report_error(ReportArgs) ->
    ok = error_logger:error_report([ mod_post_log_cannot_post | ReportArgs ]).

format_jid(#jid{luser = User, lserver = Server, lresource = Resource})
  when Resource =:= undefined;
       Resource =:= "";
       Resource =:= <<"">> ->
    %% The guard above feels defensive, but I don't yet know the full
    %% set of ways that ejabberd will represent an empty resource
    io_lib:format("~s@~s", [User, Server]);
format_jid(#jid{luser = User, lserver = Server, lresource = Resource}) ->
    io_lib:format("~s@~s/~s", [User, Server, Resource]).

%% Erlang now()-style timestamps are in UTC by definition, and we are
%% assuming ISO 8601 dates should be printed in UTC as well, so no
%% conversion necessary
%%
%% Example:
%%   {1385,388790,334905}
%%     -becomes-
%%   2013-11-25 14:13:10.334905Z
-spec to_iso_8601_date(erlang:timestamp()) -> string().
to_iso_8601_date(Timestamp) when is_tuple(Timestamp) ->
    {{Y, Mo, D}, {H, M, S}} = calendar:now_to_universal_time(Timestamp),
    {_, _, US} = Timestamp,
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
                                [Y, Mo, D, H, M, S, US])).
