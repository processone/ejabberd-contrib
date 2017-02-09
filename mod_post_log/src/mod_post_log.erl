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
	 depends/2,
	 mod_opt_type/1,
         log_user_send/1,
	 log_user_send/4,
         post_result/1]).

-include("ejabberd.hrl").
-include("xmpp.hrl").

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

depends(_Host, _Opts) ->
    [].

mod_opt_type(url) -> fun iolist_to_binary/1;
mod_opt_type(ts_header) -> fun iolist_to_binary/1;
mod_opt_type(from_header) -> fun iolist_to_binary/1;
mod_opt_type(to_header) -> fun iolist_to_binary/1;
mod_opt_type(headers) -> fun(L) when is_list(L) -> L end;
mod_opt_type(content_type) -> fun iolist_to_binary/1;
mod_opt_type(req_options) -> fun(L) when is_list(L) -> L end;
mod_opt_type(_) ->
    [url, ts_header, from_header, to_header, headers,
     content_type, req_options].

%% TODO: remove log_user_send/4 after 17.02 is released
log_user_send(Packet, C2SState, From, To) ->
    log_user_send({xmpp:set_from_to(Packet, From, To), C2SState}),
    Packet.

log_user_send({#message{type = T} = Packet, _C2SState} = Acc)
  when T == chat; T == groupchat ->
    ok = log_message(Packet),
    Acc;
log_user_send(Acc) ->
    Acc.

log_message(#message{from = From, to = To, body = Body} = Msg) ->
    case xmpp:get_text(Body) of
	<<"">> ->
	    ok;
	_ ->
	    XML = fxml:element_to_binary(xmpp:encode(Msg)),
            post_xml(From, To, XML)
    end.

post_xml(#jid{lserver = LServer} = From, To, Xml) ->
    Ts = to_iso_8601_date(os:timestamp()),

    Body = Xml,

    Url = get_opt(LServer, url),
    TsHeader = get_opt(LServer, ts_header, "X-Message-Timestamp"),
    FromHeader = get_opt(LServer, from_header, "X-Message-From"),
    ToHeader = get_opt(LServer, to_header, "X-Message-To"),
    Headers = [ {TsHeader,   Ts},
                {FromHeader, format_jid(From)},
                {ToHeader,   format_jid(To)}
                | get_opt(LServer, headers, []) ],
    ContentType = get_opt(LServer, content_type, "text/xml"),
    HttpOptions = get_opt(LServer, http_options, []),
    ReqOptions = get_opt(LServer, req_options, []),

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

get_opt(LServer, Opt) ->
    get_opt(LServer, Opt, undefined).

get_opt(LServer, Opt, Default) ->
    F = fun(Val) when is_binary(Val) -> binary_to_list(Val);
           (Val)                     -> Val
        end,
    gen_mod:get_module_opt(LServer, ?MODULE, Opt, F, Default).

report_error(ReportArgs) ->
    ok = error_logger:error_report([ mod_post_log_cannot_post | ReportArgs ]).

format_jid(JID) ->
    binary_to_list(jid:to_string(JID)).

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
