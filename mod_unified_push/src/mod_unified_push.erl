%%%----------------------------------------------------------------------
%%% File    : mod_unified_push.erl
%%% Author  : itd <itd@net.in.tum.de>
%%% Purpose : Handle UnifiedPush requests
%%% Created : 01 Feb 2025 by itd <itd@net.in.tum.de>
%%%
%%%
%%% mod_unified_push, Copyright (C) 2025   itd
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

-module(mod_unified_push).
-behaviour(gen_mod).
-author('itd@net.in.tum.de').

%% Required by ?DEBUG macros
-include("logger.hrl").

%% Required by ?T macro
-include("translate.hrl").

%% Enables calls to process/2
-include("ejabberd_http.hrl").

%% XMPP structs such as #iq
-include_lib("xmpp/include/xmpp.hrl").

%% JOSE JWK structs such as #jose_jwk
-include_lib("jose/include/jose_jwk.hrl").

%% gen_mod API callbacks
-export([
    start/2,
    stop/1,
    process/2,
    depends/2,
    mod_opt_type/1,
    mod_options/1,
    mod_doc/0
]).

%% IQ handler
-export([iq_handler/1]).

-type ttl_timeout() :: undefined | non_neg_integer().

%% API subpaths
-define(ENDPOINT_PUSH, "push").
-define(ENDPOINT_MESSAGE, "message").

%% Misc. string constants
-define(XMLNS_UNIFIED_PUSH, <<"http://gultsch.de/xmpp/drafts/unified-push">>).
-define(UP_EXPIRATION, <<"e">>).
-define(UP_OWNER, <<"o">>).
-define(UP_APPLICATION, <<"a">>).
-define(UP_INSTANCE, <<"i">>).

-define(MODULE_XMPP_UP, unifiedpush).

%% called by ejabberd_http
-spec process([binary()], http_request()) -> {integer(), [{binary(), binary()}], [binary()]}.
process(_LocalPath, #request{method = 'POST', data = <<>>}) ->
    ?DEBUG("bad POST request for ~p: no data", [_LocalPath]),
    {400, [], []};
process([<<?ENDPOINT_PUSH>>, MaybeJwtToken], #request{
    method = 'POST', data = Data, headers = Headers, host = Host
}) ->
    Ttl = get_ttl(Headers),
    Jwk = get_jwk(Host),
    validate_request(Host, Jwk, MaybeJwtToken, Data, Ttl);
process([], #request{method = 'GET'}) ->
    {200,
        [
            {<<"Content-Type">>, <<"application/json">>}
        ],
        [<<"{\"unifiedpush\":{\"version\":1}}">>]};
process([<<?ENDPOINT_MESSAGE>> | _], _Request) ->
    %% RFC8030: 6.2.
    {410, [], []};
process(_LocalPath, _Request) ->
    ?DEBUG("bad request for ~p: ~p", [_LocalPath, _Request]),
    {400, [], []}.

-spec get_ttl([{atom(), binary()}]) -> ttl_timeout().
get_ttl(Headers) ->
    try string:to_integer(proplists:get_value(<<"Ttl">>, Headers, "1")) of
        {T, <<>>} when T >= 0 -> T;
        {_, _} -> undefined
    catch
        _:_ ->
            %% assume illegal behavior
            undefined
    end.

-spec validate_request(binary(), any(), binary(), binary(), ttl_timeout()) ->
    {integer(), [{binary(), binary()}], []}.
validate_request(_Host, _Jwk, _MaybeJwtToken, _Data, undefined) ->
    {400, [], []};
validate_request(Host, Jwk, MaybeJwtToken, Data, Ttl) when Data =/= <<"">> ->
    ?DEBUG("verifying jwt validity", []),
    try jose_jwt:verify(Jwk, MaybeJwtToken) of
        {true, {jose_jwt, #{?UP_EXPIRATION := _ExpTest} = Fields}, Signature} ->
            Now = erlang:system_time(second),
            ?DEBUG("jwt verify at system timestamp ~p: ~p - ~p~n", [Now, Fields, Signature]),
            case maps:find(?UP_EXPIRATION, Fields) of
                error ->
                    ?DEBUG("rejecting jwt without exp(iry) field", []),
                    {401, [], []};
                {ok, Exp} ->
                    if
                        Exp > Now ->
                            ?DEBUG("valid request, forwarding notification: ~p", [Fields]),
                            forward_push_message(Host, MaybeJwtToken, Data, Ttl, Fields);
                        true ->
                            ?DEBUG("rejecting expired jwt: ~p > ~p", [Now, Exp]),
                            {401, [], []}
                    end
            end;
        {false, _, _} ->
            ?DEBUG("jose_jwt:verify failed for token: ~p", [MaybeJwtToken]),
            {401, [], []}
    catch
        A:B ->
            ?DEBUG(
                "jose_jwt:verify failed for JWK and token: ~p~n with error: ~p",
                [{Jwk, MaybeJwtToken}, {A, B}]
            ),
            {401, [], []}
    end;
validate_request(_Host, _Jwk, _MaybeJwtToken, _Data, _Ttl) ->
    {400, [], []}.

-spec forward_push_message(binary(), binary(), binary(), ttl_timeout(), map()) ->
    {integer(), [{binary(), binary()}], []}.
forward_push_message(Host, Jwt, Data, Ttl, #{
    ?UP_APPLICATION := Application, ?UP_INSTANCE := Instance, ?UP_OWNER := To
}) ->
    IQ = #iq{
        type = set,
        from = #jid{server = Host},
        to = jid:decode(To),
        sub_els = [
            #unified_push_push{
                application = Application,
                instance = Instance,
                data = base64:encode(Data)
            }
        ]
    },
    ejabberd_router:route_iq(
        IQ,
        fun(_Res) ->
            ?DEBUG("~p: IQ callback: ~p", [?MODULE, _Res]),
            ok
        end,
        undefined,
        Ttl
    ),
    UrlPrefix = misc:expand_keyword(<<"@HOST@">>, guess_url_prefix(any), Host),
    %% XXX(RFC8030) indicate no support for receive acks / message storage
    {201,
        [
            {<<"TTL">>, <<"0">>},
            {<<"Location">>, <<UrlPrefix/binary, "/", ?ENDPOINT_MESSAGE, "/", Jwt/binary>>}
        ],
        []}.

-spec iq_handler(iq()) -> iq().
iq_handler(
    #iq{
        type = set,
        to = #jid{server = Host},
        from = From,
        sub_els = [#unified_push_register{application = Application, instance = Instance}]
    } = IQ
) ->
    ?DEBUG("processing unified push IQ: ~p", [IQ]),
    %% TODO is non-monotonic time an issue here?
    Offset = gen_mod:get_module_opt(Host, ?MODULE, expiration),
    {OldMegaSecs, OldSecs, MicroSecs} = os:timestamp(),
    MegaSecs = OldMegaSecs + (OldSecs + Offset) div 1000_000,
    Secs = (OldSecs + Offset) rem 1000_000,
    Expiration = {MegaSecs, Secs, MicroSecs},
    Jwk = get_jwk(Host),
    %% TODO what are sensible JW[STK] for this scenario?
    Jws = #{<<"alg">> => <<"HS256">>},
    Jwt = #{
        ?UP_EXPIRATION => MegaSecs * 1_000_000 + Secs,
        ?UP_OWNER => jid:encode(From),
        ?UP_APPLICATION => Application,
        ?UP_INSTANCE => Instance
    },
    Signed = jose_jwt:sign(Jwk, Jws, Jwt),
    {#{}, CompactSigned} = jose_jws:compact(Signed),
    UrlPrefix = misc:expand_keyword(<<"@HOST@">>, guess_url_prefix(any), Host),
    xmpp:make_iq_result(IQ, #unified_push_registered{
        expiration = Expiration,
        endpoint = <<UrlPrefix/binary, "/", ?ENDPOINT_PUSH, "/", CompactSigned/binary>>
    });
iq_handler(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, _Opts) ->
    xmpp:register_codec(?MODULE_XMPP_UP),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?XMLNS_UNIFIED_PUSH, ?MODULE, iq_handler),
    ok.

-spec stop(binary()) -> ok.
stop(Host) ->
    xmpp:unregister_codec(?MODULE_XMPP_UP),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?XMLNS_UNIFIED_PUSH),
    ok.

-spec depends(binary(), gen_mod:opts()) -> [].
depends(_Host, _Opts) ->
    [].

-spec get_jwk(binary()) -> any().
get_jwk(Host) ->
    JwkOptions = gen_mod:get_module_opt(Host, ?MODULE, jwk),
    jose_jwk:from_map(JwkOptions).

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(expiration) ->
    %% TODO is there an upper bound for the validity of the JWT token?
    econf:int(0, 86400); %% 60 * 60 * 24
mod_opt_type(jwk) ->
    econf:map(econf:binary(), econf:either(econf:binary(), econf:int())).

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(_Host) ->
    [
        {expiration, 60 * 15},
        {jwk, jose_jwk:to_map(jose_jwk:generate_key({oct, 128}))}
    ].

%% Based on mod_host_meta's `get_auto_url` and `find_handler_port_path` functions
-spec guess_url_prefix(any | boolean()) -> undefined | binary().
guess_url_prefix(Tls) ->
    case
        lists:filtermap(
            fun
                (
                    {{Port, _, _}, ejabberd_http, #{tls := ThisTls, request_handlers := Handlers}}
                ) when
                    (Tls == any) or (Tls == ThisTls)
                ->
                    case lists:keyfind(?MODULE, 2, Handlers) of
                        false -> false;
                        {Path, ?MODULE} -> {true, {ThisTls, Port, Path}}
                    end;
                (_) ->
                    false
            end,
            ets:tab2list(ejabberd_listener)
        )
    of
        [] ->
            undefined;
        [{ThisTls, Port, Path} | _] ->
            Protocol =
                case ThisTls of
                    false -> <<"http">>;
                    true -> <<"https">>
                end,
            <<Protocol/binary, "://@HOST@:", (integer_to_binary(Port))/binary, "/",
                (str:join(Path, <<"/">>))/binary>>
    end.

-spec mod_doc() -> map().
mod_doc() ->
    #{
        desc => ?T("UnifiedPush push server implementation."),
        note => "experimental",
        opts => [
            {expiration, #{
                value => ?T("Number"),
                desc =>
                    ?T(
                        "Every generated push URL will be valid only "
                        "for the specified interval, in seconds."
                    )
            }},
            {jwk, #{
                value => ?T("Map"),
                desc =>
                    ?T(
                        "Passed to `jose_jwk:from_map` to generate the "
                        "JSON Web Key (JWK) used when generating URLs."
                    )
            }}
        ],
        example => [
            "listen:",
            "  -",
            "    port: 5222",
            "    module: ejabberd_c2s",
            "  -",
            "    port: 5443",
            "    module: ejabberd_http",
            "    request_handlers:",
            "      /up: mod_unified_push",
            "",
            "modules:",
            "  mod_unified_push:",
            "    expiration: 120",
            "    jwk: {\"k\" => \"a4-...\",\"kty\" => \"oct\"}"
        ]
    }.
