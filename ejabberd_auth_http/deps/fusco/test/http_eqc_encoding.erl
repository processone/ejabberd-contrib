%%%=============================================================================
%%% @copyright (C) 1999-2014, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(http_eqc_encoding).
-copyright("2014, Erlang Solutions Ltd.").

-export([add_content_length/2,
         add_transfer_encoding/2,
         body/1]).

-export([build_valid_response/4,
         build_cookie/1,
         build_expires_date/1,
         expires_datetime/1]).

%%==============================================================================
%% API
%%==============================================================================
add_content_length(Headers, <<>>) ->
    Headers;
add_content_length(Headers, Body) ->
    ContentLength = list_to_binary(integer_to_list(byte_size(Body))),
    [{<<"Content-Length">>, ContentLength} | Headers].

body({_, <<$1, _, _>>, _}) ->
    <<>>;
body({_, <<$2,$0,$4>>, _}) ->
    <<>>;
body({_, <<$3,$0,$4>>, _}) ->
    <<>>;
body(_) ->
    http_eqc_gen:body().

add_transfer_encoding(Headers, <<>>) ->
    Headers;
add_transfer_encoding(Headers, Encoding) ->
    lists:keystore(<<"Transfer-Encoding">>, 1,
		   remove_transfer_encoding(Headers),
		   {<<"Transfer-Encoding">>, Encoding}).

build_valid_response({HttpVersion, StatusCode, Reason}, Headers, Cookies, Body) ->
    SL = [HttpVersion, sp(), StatusCode, sp(), Reason, crlf()],
    HS = [[Name, colon(), Value, crlf()] || {Name, Value} <- Headers],
    CS = [[Name, colon(), build_cookie(Cookie), crlf()] || {Name, Cookie} <- Cookies],
    [SL, HS ++ CS, crlf(), build_body(Body)].

build_cookie({{K, V}, Avs}) ->
    CookiePair = [K, eq(), V],
    CookieAvs = build_cookie_avs(Avs),
    CookiePair ++ CookieAvs.

build_expires_date({rfc1123date, {Wkday, Date1, Time}}) ->
    Date = build_date(Date1),
    BTime = build_time(Time),
    [Wkday, $,, $\s, Date, $\s, BTime, $\s, "GMT"];
build_expires_date({rfc850date, {Weekday, Date2, Time}}) ->
    Date = build_date(Date2),
    BTime = build_time(Time),
    [Weekday, $,, $\s, Date, $\s, BTime, $\s, "GMT"];
build_expires_date({asctimedate, {Wkday, Date3, Time, Year}}) ->
    BTime = build_time(Time),
    Date = build_date(Date3),
    [Wkday, $\s, Date, $\s, BTime, $\s, Year].

expires_datetime({rfc1123date, {_, {date1, {Day, Month, Year}}, {H, M, S}}}) ->
    {{st_to_int(Year), month(Month), st_to_int(Day)},
     {st_to_int(H), st_to_int(M), st_to_int(S)}};
expires_datetime({rfc850date, {_, {date2, {Day, Month, Year}}, {H, M, S}}}) ->
    {{to_year(Year), month(Month), st_to_int(Day)},
     {st_to_int(H), st_to_int(M), st_to_int(S)}};
expires_datetime({asctimedate, {_, {date3, {Day, Month}}, {H, M, S}, Year}}) ->
    {{st_to_int(Year), month(Month), st_to_int(Day)},
     {st_to_int(H), st_to_int(M), st_to_int(S)}};
expires_datetime(undefined) ->
    undefined.
%%==============================================================================
%% Internal functions
%%==============================================================================    
remove_transfer_encoding(Headers) ->
    lists:filter(fun({H, _}) -> H =/= <<"Transfer-Encoding">> end, Headers).
    
build_body(Body) when is_binary(Body) ->
    Body;
build_body(List) ->
    list_to_binary(
      io_lib:format("~s0\r\n\r\n",
		    [[io_lib:format("~s\r\n~s\r\n",
				    [erlang:integer_to_list(Nat, 16), Body])
		      || {Nat, Body} <- List]])).

build_cookie_avs(Avs) ->
    build_cookie_avs(Avs, []).
    
build_cookie_avs([{<<"Expires">> = K, Date} | Rest], Acc) ->
    V = build_expires_date(Date),
    build_cookie_avs(Rest, [[semicolon(), sp(), K, eq(), V] | Acc]);
build_cookie_avs([{K, V} | Rest], Acc) ->
    build_cookie_avs(Rest, [[semicolon(), sp(), K, eq(), V] | Acc]);
build_cookie_avs([K | Rest], Acc) ->
    build_cookie_avs(Rest, [[semicolon(), sp(), K] | Acc]);
build_cookie_avs([], Acc) ->
    Acc.

build_date({date1, {Day, Month, Year}}) ->
    [Day, $\s, Month, $\s, Year];
build_date({date2, {Day, Month, Year}}) ->
    [Day, $-, Month, $-, Year];
build_date({date3, {Day, Month}}) ->
    [Month, $\s, Day].

build_time({H, M, S}) ->
    [H, $:, M, $:, S].

colon() ->
    <<$:>>.

semicolon() ->
    <<$;>>.

sp() ->
    <<$\s>>.

crlf() ->
    <<$\r,$\n>>.

eq() ->
    <<$=>>.

months() ->
    [{"Jan", 1}, {"Feb", 2}, {"Mar", 3}, {"Apr", 4},
     {"May", 5}, {"Jun", 6}, {"Jul", 7}, {"Aug", 8},
     {"Sep", 9}, {"Oct", 10}, {"Nov", 11}, {"Dec", 12}].

st_to_int(L) ->
    list_to_integer(L).

month(Month) ->
    proplists:get_value(Month, months()).

to_year(Year) when length(Year) == 4 ->
    st_to_int(Year);
to_year(Year) ->
    IntY = list_to_integer(Year),
    {Y, _, _} = date(),
    case (2000 + IntY - Y) > 50 of
        true ->
            1900 + IntY;
        false ->
            2000 + IntY
    end.
