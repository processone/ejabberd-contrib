%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc Quickcheck generators for HTTP messages
%%% @end
%%%=============================================================================
-module(http_eqc_gen).
-copyright("2013, Erlang Solutions Ltd.").

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% RFC 2616
general_header() ->
    [{<<"Cache-Control">>, small_valid_bin()},
     {<<"Connection">>, connection_header()},
     {<<"Date">>, small_valid_bin()},
     {<<"Pragma">>, small_valid_bin()},
     {<<"Trailer">>, small_valid_bin()},
     {<<"Transfer-Encoding">>, small_valid_bin()},
     {<<"Upgrade">>, small_valid_bin()},
     {<<"Via">>, small_valid_bin()},
     {<<"Warning">>, small_valid_bin()}].

connection_header() ->
    oneof([<<"close">>, <<"keep-alive">>, small_valid_bin()]).

%% RFC 2616
entity_header() ->
    [{<<"Allow">>, small_valid_bin()},
     {<<"Content-Encoding">>, small_valid_bin()},
     {<<"Content-Language">>, small_valid_bin()},
     {<<"Content-Location">>, small_valid_bin()},
     {<<"Content-MD5">>, small_valid_bin()},
     {<<"Content-Range">>, small_valid_bin()},
     {<<"Content-Type">>, small_valid_bin()},
     {<<"Expires">>, small_valid_bin()},
     {<<"Last-Modified">>, small_valid_bin()}].

%% RFC 2616
response_header() ->
    [{<<"Accept-Ranges">>, small_valid_bin()},
     {<<"Age">>, small_valid_bin()},
     {<<"ETag">>, small_valid_bin()},
     {<<"Location">>, small_valid_bin()},
     {<<"Proxy-Authenticate">>, small_valid_bin()},
     {<<"Retry-After">>, small_valid_bin()},
     {<<"Server">>, small_valid_bin()},
     {<<"Vary">>, small_valid_bin()},
     {<<"WWW-Authenticate">>, small_valid_bin()}].

%% http://tools.ietf.org/html/rfc2616#section-5.3
request_header() ->
    [{<<"Accept">>, small_valid_bin()},
     {<<"Accept-Charset">>, small_valid_bin()},
     {<<"Accept-Encoding">>, small_valid_bin()},
     {<<"Accept-Language">>, small_valid_bin()},
     {<<"Authorization">>, authorization()},
     {<<"Expect">>, small_valid_bin()},
     {<<"From">>, small_valid_bin()},
     {<<"Host">>, small_valid_bin()},
     {<<"If-Match">>, small_valid_bin()},
     {<<"If-Modified-Since">>, small_valid_bin()},
     {<<"If-None-Match">>, small_valid_bin()},
     {<<"If-Range">>, small_valid_bin()},
     {<<"If-Unmodified-Since">>, small_valid_bin()},
     {<<"Max-Forwards">>, small_valid_bin()},
     {<<"Proxy-Authorization">>, small_valid_bin()},
     {<<"Range">>, small_valid_bin()},
     {<<"Referer">>, small_valid_bin()},
     {<<"TE">>, small_valid_bin()},
     {<<"User-Agent">>, small_valid_bin()}
    ].

authorization() ->
    ?LET({User, Pass}, {small_valid_bin(), small_valid_bin()},
	 begin
	     Encoded = base64:encode(<<User/binary, $:, Pass/binary>>),
	     <<"Basic ", Encoded/binary>>
	 end).

header() ->
    lists:append([general_header(), entity_header(), response_header()]).

req_headers() ->
    lists:append([general_header(), entity_header(), request_header()]).

request_headers() ->
    ?LET(Headers, list(oneof(req_headers())), Headers).

headers() ->
    ?LET(Headers, list(oneof(header())), Headers).

http_version() ->
    [<<"HTTP/1.0">>, <<"HTTP/1.1">>].

informational_code() ->
    [{<<"100">>, <<"Continue">>},
     {<<"101">>, <<"Switching protocols">>}].

success_code() ->
    [{<<"200">>, <<"OK">>},
     {<<"201">>, <<"Created">>},
     {<<"202">>, <<"Accepted">>},
     {<<"203">>, <<"Non-Authoritative Information">>},
     {<<"204">>, <<"No Content">>},
     {<<"205">>, <<"Reset Content">>},
     {<<"206">>, <<"Partial Content">>}].

redirection_code() ->
    [{<<"300">>, <<"Multiple Choices">>},
     {<<"301">>, <<"Moved Permanently">>},
     {<<"302">>, <<"Found">>},
     {<<"303">>, <<"See Other">>},
     {<<"304">>, <<"Not Modified">>},
     {<<"305">>, <<"Use Proxy">>},
     {<<"307">>, <<"Temporary Redirect">>}].

client_error_code() ->
    [{<<"400">>, <<"Bad Request">>},
     {<<"401">>, <<"Unauthorized">>},
     {<<"402">>, <<"Payment Required">>},
     {<<"403">>, <<"Forbidden">>},
     {<<"404">>, <<"Not Found">>},
     {<<"405">>, <<"Method Not Allowed">>},
     {<<"406">>, <<"Not Acceptable">>},
     {<<"407">>, <<"Proxy Authentication Required">>},
     {<<"408">>, <<"Request Time-out">>},
     {<<"409">>, <<"Conflict">>},
     {<<"410">>, <<"Gone">>},
     {<<"411">>, <<"Length Required">>},
     {<<"412">>, <<"Precondition Failed">>},
     {<<"413">>, <<"Request Entity Too Large">>},
     {<<"414">>, <<"Request-URI Too Large">>},
     {<<"415">>, <<"Unsupported Media Type">>},
     {<<"416">>, <<"Requested range not satisfiable">>},
     {<<"417">>, <<"Expectation Failed">>}].

server_error_code() ->
    [{<<"500">>, <<"Internal Server Error">>},
     {<<"501">>, <<"Not Implemented">>},
     {<<"502">>, <<"Bad Gateway">>},
     {<<"503">>, <<"Service Unavailable">>},
     {<<"504">>, <<"Gateway Time-out">>},
     {<<"505">>, <<"HTTP Version not supported">>}].

%% RFC 6265
set_cookie() ->
    {<<"Set-Cookie">>, set_cookie_string()}.

set_cookie_string() ->
    {cookie_pair(), cookie_avs()}.

cookie_pair() ->
    {small_valid_bin(), small_valid_bin()}.

cookie_avs() ->
    list(oneof(cookie_av())).

cookie_av() ->
    [{<<"Expires">>, sane_cookie_date()},
     {<<"Max-Age">>, max_age()},
     {<<"Domain">>, small_valid_bin()},
     {<<"Path">>, small_valid_bin()},
     {<<"Secure">>, small_valid_bin()},
     <<"HttpOnly">>,
     small_valid_bin() %% extension
    ].

sane_cookie_date() ->
    ?LET(Date, oneof([rfc1123date(), rfc850date(), asctimedate()]), Date).

max_age() ->
    ?LET(Age, ?SUCHTHAT(Nat, nat(), Nat > 0), list_to_binary(integer_to_list(Age))).

rfc1123date() ->
    {rfc1123date, {wkday(), date1(), timeb()}}.

rfc850date() ->
    {rfc850date, {weekday(), date2(), timeb()}}.

asctimedate() ->
    {asctimedate, {wkday(), date3(), timeb(), year4()}}.

date1() ->
    {date1, {day(), month(), year4()}}.

date2() ->
    {date2, {day(), month(), year2()}}.

date3() ->
    {date3, {day(), month()}}.

timeb() ->
    ?LET({H, M, S}, {choose(0, 23), choose(0,59), choose(0, 59)},
	 {twod(H), twod(M), twod(S)}).

twod(Integer) ->
    string:right(integer_to_list(Integer), 2, $0).

day() ->
    ?LET(Day, choose(1, 31), twod(Day)).

year4() ->
    ?LET(Year, choose(1983, 2083), integer_to_list(Year)).

year2() ->
    ?LET(Year, choose(0, 99), twod(Year)).

wkday() ->
    ?LET(Wkday, oneof(["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]), Wkday).

weekday() ->
    ?LET(Weekday, oneof(["Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
			 "Saturday", "Sunday"]),
	 Weekday).

month() ->
    ?LET(Month, 
	 oneof(["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
		"Oct", "Nov", "Dec"]),
	 Month).

status_code() ->
    lists:append([informational_code(), success_code(), redirection_code(),
		  client_error_code(), server_error_code()]).

status_line() ->
    ?LET({HttpVersion, {StatusCode, Reason}},
	 {oneof(http_version()), oneof(status_code())},
	 {HttpVersion, StatusCode, Reason}).

http_method() ->
    ["OPTIONS", "GET", "HEAD", "POST", "PUT", "DELETE", "TRACE", "CONNECT"].

request_uri() ->
    [<<"*">>, <<"http://www.w3.org/pub/WWW/TheProject.html">>,
     <<"/pub/WWW/TheProject.html">>].

request_line() ->
    ?LET({Method, RequestUri},
	 {oneof(http_method()), oneof(request_uri())},
	 {Method, RequestUri, "HTTP/1.1"}).

small_valid_bin() ->
    ?LET(String, vector(5, choose($A, $z)),
	 list_to_binary(String)).

body() ->
    ?LET(String, list(choose($A, $z)),
	 list_to_binary(String)).

body(Size) ->
    ?LET(String, vector(Size, choose($A, $z)), String).

chunked_body() ->
    ?LET(PosNats, non_empty(list(?SUCHTHAT(Nat, nat(), Nat > 0))),
	 ?LET(Bins, [body(Size) || Size <- PosNats],
	      lists:zip(PosNats, Bins)
	     )
	).
