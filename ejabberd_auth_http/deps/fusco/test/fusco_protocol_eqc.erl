%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(fusco_protocol_eqc).
-copyright("2013, Erlang Solutions Ltd.").

-include_lib("eqc/include/eqc.hrl").
-include("fusco.hrl").

-export([prop_http_response_close_connection/0,
	 prop_http_response_keep_alive/0,
	 prop_chunked_http_response_keep_alive/0]).

%%==============================================================================
%% Quickcheck generators
%%==============================================================================
valid_http_message() ->
    ?LET({StatusLine, Headers, Cookies},
	 {http_eqc_gen:status_line(), http_eqc_gen:headers(),
	  list(http_eqc_gen:set_cookie())},
	 ?LET(Body, http_eqc_encoding:body(StatusLine),
	      {StatusLine, http_eqc_encoding:add_content_length(Headers, Body),
	       Cookies, Body})).

valid_http_chunked_message() ->
    ?LET({StatusLine, Headers, Cookies},
	 {http_eqc_gen:status_line(), http_eqc_gen:headers(),
	  list(http_eqc_gen:set_cookie())},
	 ?LET(Body, http_eqc_gen:chunked_body(),
	      {StatusLine, http_eqc_encoding:add_transfer_encoding(
			     Headers, <<"chunked">>),
	       Cookies, Body})).

%%==============================================================================
%% Quickcheck properties
%%==============================================================================
prop_http_response_close_connection() ->
    %% Connection is closed just after send the response
    prop_http_response(close).

prop_http_response_keep_alive() ->
    %% Connection stays open after send the response
    prop_http_response(keepalive).

prop_chunked_http_response_keep_alive() ->
    %% Connection stays open after send the response
    prop_chunked_http_response(keepalive).

prop_http_response(ConnectionState) ->
    eqc:numtests(
      500,
      ?FORALL(ValidMessage, valid_http_message(),
	      decode_valid_message(ConnectionState, ValidMessage))).

decode_valid_message(ConnectionState, {StatusLine, Headers, Cookies, Body}) ->
    Msg = http_eqc_encoding:build_valid_response(StatusLine, Headers, Cookies, Body),
    L = {_, _, Socket} =
	test_utils:start_listener({fragmented, Msg}, ConnectionState),
    test_utils:send_message(Socket),
    Recv = fusco_protocol:recv(Socket, false),
    test_utils:stop_listener(L),
    Expected = expected_output(StatusLine, Headers, Cookies, Body, Msg),
    Cleared = clear_record(clear_connection(Recv)),
    ?WHENFAIL(io:format("Message:~n=======~n~s~n=======~nResponse:"
			" ~p~nCleared: ~p~nExpected: ~p~n",
			[binary:list_to_bin(Msg), Recv, Cleared, Expected]),
	      case Cleared of
		  Expected ->
		      true;
		  _ ->
		      false
	      end).

prop_chunked_http_response(ConnectionState) ->
    eqc:numtests(
      500,
      ?FORALL(ValidMessage, valid_http_chunked_message(),
	      decode_valid_message(ConnectionState, ValidMessage))).

%%==============================================================================
%% Internal functions
%%==============================================================================
expected_output({HttpVersion, StatusCode, Reason}, Headers, Cookies, Body, Msg) ->
    Version = http_version(HttpVersion),
    OCookies = [{Name, list_to_binary(http_eqc_encoding:build_cookie(Cookie))}
                || {Name, Cookie} <- Cookies],
    LowerHeaders = lists:reverse(headers_to_lower(Headers ++ OCookies)),
    CookiesRec = output_cookies(Cookies),    
    #response{version = Version,
	      status_code = StatusCode,
	      reason = Reason,
	      cookies = CookiesRec,
	      headers = LowerHeaders,
	      connection = to_lower(proplists:get_value(<<"connection">>,
							LowerHeaders)),
	      body = expected_body(Body),
	      content_length = content_length(Body),
	      transfer_encoding = to_lower(proplists:get_value(<<"transfer-encoding">>,
							       LowerHeaders)),
	      size = byte_size(list_to_binary(Msg))}.

expected_body(Body) when is_binary(Body) ->
    Body;
expected_body(List) ->
    list_to_binary([Bin || {_, Bin} <- List]).

content_length(Body) when is_binary(Body) ->
    byte_size(Body);
content_length(_) ->
    0.

output_cookies(Cookies) ->
    output_cookies(Cookies, []).

output_cookies([{_SetCookie, {{K, V}, Avs}} | Rest], Acc) ->
    MaxAge = output_max_age(proplists:get_value(<<"Max-Age">>, Avs)),
    Path = proplists:get_value(<<"Path">>, Avs),
    PathTokens = case Path of
                     Bin when is_binary(Bin) ->
                         binary:split(Bin, <<"/">>, [global]);
                     undefined ->
                         undefined
                 end,
    Expires = http_eqc_encoding:expires_datetime(proplists:get_value(<<"Expires">>, Avs)),
    Cookie = #fusco_cookie{name = K, value = V, max_age = MaxAge, path = Path,
                           path_tokens = PathTokens,
                           expires = Expires,
                           domain = proplists:get_value(<<"Domain">>, Avs)},
    output_cookies(Rest, [Cookie | Acc]);
output_cookies([], Acc) ->
    Acc.

output_max_age(undefined) ->
    undefined;
output_max_age(Age) ->
    list_to_integer(binary_to_list(Age)) * 1000000.

clear_record(Response) when is_record(Response, response) ->
    Response#response{socket = undefined,
		      ssl = undefined,
		      in_timestamp = undefined};
clear_record(Error) ->
    Error.

clear_connection(Response) when is_record(Response, response) ->
    Response#response{connection = to_lower(Response#response.connection)};
clear_connection(Error) ->
    Error.

http_version(<<"HTTP/1.1">>) ->
    {1, 1};
http_version(<<"HTTP/1.0">>) ->
    {1, 0}.

headers_to_lower(Headers) ->
    [begin
	 He = to_lower(H),
	 case He of
	     LH when LH == <<"connection">>; LH == <<"transfer-encoding">> ->
		 {He, to_lower(V)};
	     _ ->
		 {He, V}
	 end
     end || {H, V} <- Headers].

to_lower(undefined) ->
    undefined;
to_lower(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).

