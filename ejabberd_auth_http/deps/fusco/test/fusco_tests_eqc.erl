%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(fusco_tests_eqc).
-copyright("2013, Erlang Solutions Ltd.").

-include_lib("eqc/include/eqc.hrl").
-include("fusco.hrl").

-define(TWO_OK, <<"HTTP/1.1 200 OK\r\n\r\n">>).
-define(FOUR_BAD_REQUEST, <<"HTTP/1.1 400 Bad Request\r\n">>).
-define(TWO_OK(V), case V of
		       undefined ->
			   ?TWO_OK;
		       _ ->
			   <<"HTTP/1.1 200 OK\r\nConnection: ",
			     V/binary,"\r\n\r\n">>
		   end).

-export([prop_http_request_per_family/3,
         prop_persistent_connection_per_family/3,
         prop_reconnect_per_family/3,
         prop_client_close_connection_per_family/3,
         prop_connection_refused_per_family/3,
         prop_http_request_cookie_path/3,
         prop_http_request_supersede_cookie/3,
         prop_http_request_max_age/3,
         prop_http_request_expires/3]).

%%==============================================================================
%% Quickcheck generators
%%==============================================================================
valid_http_request() ->
    ?LET({RequestLine, Headers},
	 {http_eqc_gen:request_line(), http_eqc_gen:request_headers()},
	 ?LET(Body, http_eqc_encoding:body(any),
	      {RequestLine, http_eqc_encoding:add_content_length(Headers, Body),
	       Body})).

valid_http_response() ->
    ?LET({StatusLine, Headers},
         {status_line(), http_eqc_gen:headers()},
         ?LET(Body, http_eqc_encoding:body(StatusLine),
              {StatusLine, Headers, Body}
             )
        ).

status_line() ->
    %% Discard CONTINUE for cookie testing, client waits for next messages
    ?SUCHTHAT({_, S, _}, http_eqc_gen:status_line(),
              not lists:member(S, [<<"100">>, <<"101">>])).

token() ->
    non_empty(list(choose($A, $z))).

path() ->
    ?LET({Path, Slash}, {non_empty(list(token())), slash()},
         Path ++ Slash).

domain() ->
    ?LET(Domain, path(), list_to_binary(string:join(Domain, "."))).

slash() ->
    oneof([["/"], []]).

subpath(Path, true) ->
    ?LET({Length, Slash}, {choose(1, length(Path)), slash()},
         begin
             {H, _} = lists:split(Length, Path),
             case lists:last(H) of
                 "/" ->
                     H;
                 _ ->
                     H ++ Slash
             end
         end);
subpath(Path, false) ->
    ?SUCHTHAT(SubPath, path(), hd(SubPath) =/= hd(Path)).

max_age() ->
    %% Make cookie expire on nat/0 and not expire on largeint/0
    %% Otherwise, in black box testing we lose the control to make cookie
    %% expire on values near the current time. It needs unit testing
    %% to verify the expiration is precise.
    ?LET({Expires, MaxAge},
         oneof([{true, nat()}, {false, largeint()}]),
         case MaxAge of
             0 ->
                 {true, MaxAge};
             _ ->
                 {Expires, abs(MaxAge)}
         end).

past() ->
    ?SUCHTHAT(Date, http_eqc_gen:sane_cookie_date(), is_past(Date)).

future() ->
    ?SUCHTHAT(Date, http_eqc_gen:sane_cookie_date(), is_future(Date)).

expires() ->
    oneof([{true, past()}, {false, future()}]).

set_cookie(Path) ->
    {<<"Set-Cookie">>, {http_eqc_gen:cookie_pair(),
                        [{<<"Path">>, encode_path(Path)}]}}.

set_cookie(Path, MaxAge) when is_integer(MaxAge) ->
    {<<"Set-Cookie">>, {http_eqc_gen:cookie_pair(),
                        [{<<"Path">>, encode_path(Path)},
                         {<<"Max-Age">>, integer_to_binary(MaxAge)}]}};
set_cookie(Path, Expires) ->
    {<<"Set-Cookie">>,
     {http_eqc_gen:cookie_pair(),
      [{<<"Path">>, encode_path(Path)},
       {<<"Expires">>, Expires}]}}.

set_cookie(Path, Domain, Name, Value) ->
    {<<"Set-Cookie">>, {{Name, Value}, [{<<"Path">>, encode_path(Path)},
                                        {<<"Domain">>, Domain}]}}.

cookie_path() ->
    %% Cookie rejected if the value for the Path attribute
    %% is not a prefix of the requested-URI
    ?LET({Path, IsSubPath}, {path(), bool()},
         ?LET(SubPath, subpath(Path, IsSubPath),
              ?LET(Cookie, set_cookie(SubPath),
                   {Cookie, encode_path(Path), IsSubPath}
                  )
             )
        ).

cookie_max_age() ->
    %% Path is needed in the test setup to ensure cookie is not rejected
    ?LET({Path, {Expires, MaxAge}}, {path(), max_age()},
         ?LET(Cookie, set_cookie(Path, MaxAge),
              {Cookie, encode_path(Path), Expires, MaxAge}
             )
        ).

cookie_expires() ->
    %% Path is needed in the test setup to ensure cookie is not rejected
    ?LET({Path, {Expires, Date}}, {path(), expires()},
         ?LET(Cookie, set_cookie(Path, Date),
              {Cookie, encode_path(Path), Expires}
             )
        ).

maybe_different_cookie_data(true, Path, Domain, Name) ->
    {Path, Domain, Name};
maybe_different_cookie_data(false, Path, Domain, Name) ->
    %% Generates a combination of 1 or more mutations on Path, Domain and Name
    ?LET([ChangePath, ChangeDomain, ChangeName],
         ?SUCHTHAT(V, vector(3, bool()),
                   lists:any(fun(X) -> X == true end, V)),
         {change_path(ChangePath, Path),
          change_domain(ChangeDomain, Domain),
          change_name(ChangeName, Name)}
        ).

change_path(false, Path) ->
    Path;
change_path(true, Path) ->
    ?SUCHTHAT(P, path(), hd(P) =/= hd(Path)).

change_domain(false, Domain) ->
    Domain;
change_domain(true, Domain) ->
    ?SUCHTHAT(D, domain(), D =/= Domain).

change_name(false, Name) ->
    Name;
change_name(true, Name) ->
    ?SUCHTHAT(N, http_eqc_gen:small_valid_bin(), N =/= Name).

%%==============================================================================
%% Quickcheck properties
%%==============================================================================
prop_http_request_per_family(Host, Family, Ssl) ->
    eqc:numtests(
      500,
      ?FORALL({{Method, Uri, _Version}, Headers, Body} = Msg,
	      valid_http_request(),
	      begin
		  Module = select_module(Ssl),
		  {ok, Listener, LS, Port} =
		      webserver:start(Module, [validate_msg(Msg)], Family),
		  {ok, Client} = fusco:start({Host, Port, Ssl}, []),
		  {ok, {Status, _, _, _, _}}
		      = fusco:request(Client, Uri, Method, Headers, Body, 10000), 
		  ok = fusco:disconnect(Client),
		  webserver:stop(Module, Listener, LS),
		  Expected = {<<"200">>, <<"OK">>},
		  ?WHENFAIL(io:format("Status: ~p~nExpected: ~p~n",
				      [Status, Expected]),
			    case Status of
				Expected ->
				    true;
				_ ->
				    false
			    end)
	      end)).

prop_persistent_connection_per_family(Host, Family, Ssl) ->
    %% Fusco must keep the connection alive and be able to reconnect
    %% Individual properties defined for reconnect and keep-alive
    ?FORALL(
       Msgs,
       non_empty(list({valid_http_request(), http_eqc_gen:connection_header()})),
       begin
	   Module = select_module(Ssl),
	   {ok, Listener, LS, Port} =
	       webserver:start(Module,
			       [reply_msg(?TWO_OK(ConHeader)) || {_, ConHeader} <- Msgs],
			       Family),
	   {ok, Client} = fusco:start({Host, Port, Ssl}, []),
	   Replies = lists:map(fun({{{Method, Uri, _Version}, Headers, Body}, _}) ->
				       fusco:request(Client, Uri, Method, Headers, Body, 10000)
			       end, Msgs),
	   ok = fusco:disconnect(Client),
	   webserver:stop(Module, Listener, LS),
	   ?WHENFAIL(io:format("Replies: ~p~nExpected: 200 OK~n", [Replies]),
		     lists:all(fun({ok, {{<<"200">>, <<"OK">>}, _, _, _, _}}) ->
				       true;
				  (_) ->
				       false
			       end, Replies))
       end).

prop_reconnect_per_family(Host, Family, Ssl) ->
    %% Connection is always closed in the server and fusco must reconnect
    eqc:numtests(
      50,
      ?FORALL(
	 Msgs,
	 non_empty(list(valid_http_request())),
	 begin
	     Module = select_module(Ssl),
	     {ok, Listener, LS, Port} =
		 webserver:start(Module,
				 [reply_and_close_msg(?TWO_OK) || _ <- Msgs],
				 Family),
	     {ok, Client} = fusco:start({Host, Port, Ssl}, []),
	     Replies = lists:map(fun({{Method, Uri, _Version}, Headers, Body}) ->
					 Hdrs = lists:keydelete(<<"Connection">>, 1, Headers),
					 fusco:request(Client, Uri, Method, Hdrs, Body, 10000)
				 end, Msgs),
	     ok = fusco:disconnect(Client),
	     webserver:stop(Module, Listener, LS),
	     ?WHENFAIL(io:format("Replies: ~p~nExpected: 200 OK~n", [Replies]),
		       lists:all(fun({ok, {{<<"200">>, <<"OK">>}, _, _, _, _}}) ->
					 true;
				    (_) ->
					 false
				 end, Replies))
	 end)).

prop_client_close_connection_per_family(Host, Family, Ssl) ->
    %% Fusco must close the connection if requested by the server
    eqc:numtests(
      25,
      ?FORALL({{{Method, Uri, _Version}, Headers, Body}, Connection},
	      {valid_http_request(), http_eqc_gen:connection_header()},
	      begin
		  Id = erlang:now(),
		  Module = select_module(Ssl),
		  {ok, Listener, LS, Port} =
		      webserver:start(Module,
				      [reply_msg_and_check(Id, ?TWO_OK(Connection))],
				      Family),
		  {ok, Client} = fusco:start({Host, Port, Ssl}, []),
		  {ok, {Status, _, _, _, _}}
		      = fusco:request(Client, Uri, Method, Headers, Body, 10000), 
		  Closed = receive
			       {Id, closed} ->
				  true
			   after 1000 ->
				   false
			   end,
		  ok = fusco:disconnect(Client),
		  webserver:stop(Module, Listener, LS),
		  Expected = {<<"200">>, <<"OK">>},
		  MustClose = must_close(Headers, Connection),
		  ?WHENFAIL(io:format("Connection: ~p~nStatus: ~p~nExpected:"
				      " ~p~nMust close: ~p~nClosed: ~p~n",
				      [Connection, Status, Expected, MustClose, Closed]),
			    case Status of
				Expected ->
				    MustClose == Closed;
				_ ->
				    false
			    end)
	      end)).

prop_connection_refused_per_family(Host, Family, Ssl) ->
    eqc:numtests(1,
      begin
	  Module = select_module(Ssl),
	  {ok, Listener, LS, Port} =
	      webserver:start(Module, [reply_msg(<<>>)], Family),
	  webserver:stop(Module, Listener, LS),
	  {ok, Client} = fusco:start({Host, Port, Ssl}, []),
	  Reply = fusco:connect(Client),
	  Expected = {error, econnrefused},
	  ?WHENFAIL(io:format("Reply: ~p~nExpected: ~p~n",
			      [Reply, Expected]),
		    case Reply of
			Expected ->
			    true;
			_ ->
			    false
		    end)
      end).

%% Cookie rejected if the value for the Path attribute
%% is not a prefix of the requested-URI
prop_http_request_cookie_path(Host, Family, Ssl) ->
    ?FORALL(
       {Request, {Cookie, Path, IsSubPath},
        {{_, Status, Reason}, _, _} = Response},
       {valid_http_request(), cookie_path(), valid_http_response()},
       begin
           ResponseBin = build_response(Response, [Cookie]),
           ValidationFun = validate_cookie(ResponseBin, fun check_cookie_deleted/3,
                                           [not IsSubPath, Cookie]),
           Responses =
               send_cookie_requests(Host, Ssl, Family, ValidationFun, Path, Request,
                                    [<<"first">>, <<"second">>], 0),
           check_responses(Status, Reason, Responses)
       end
      ).

%% Supersed old cookie if Name is the same as existing cookie,
%% and Domain and Path exactly match pre-existing ones
prop_http_request_supersede_cookie(Host, Family, Ssl) ->
    ?FORALL(
       {Request, Path, Domain, {Name, Value}, Supersede,
        {{_, Status, Reason}, _, _} = Response},
       {valid_http_request(), path(), domain(), http_eqc_gen:cookie_pair(),
        bool(), valid_http_response()},
       %% Generate a second cookie that could supersed or not the previous one
       %% Uses 'Supersede' as generation parameter
       ?LET(
          {{SPath, SDomain, SName}, SValue},
          {maybe_different_cookie_data(Supersede, Path, Domain, Name),
           ?SUCHTHAT(V, http_eqc_gen:small_valid_bin(), V =/= Value)},
          begin
              FirstCookie = set_cookie(Path, Domain, Name, Value),
              SecondCookie = set_cookie(SPath, SDomain, SName, SValue),
              FirstServerResponse = build_response(Response, [FirstCookie]),
              SecondServerResponse = build_response(Response, [SecondCookie]),
              ValidationFun = validate_cookie_supersede(
                                FirstServerResponse, SecondServerResponse,
                                Supersede, {Name, Value, encode_path(Path), Domain},
                                {SName, SValue, encode_path(SPath), SDomain}),
              %% Three requests to supersede the value
              %% First get cookie
              %% Second get second cookie
              %% Third checks received cookies
              Responses =
                  send_cookie_requests(Host, Ssl, Family, ValidationFun,
                                       encode_path(Path), Request,
                                       [<<"first">>, <<"second">>, <<"third">>], 0),
              check_responses(Status, Reason, Responses)
          end
         )
      ).

prop_http_request_max_age(Host, Family, Ssl) ->
    eqc:numtests(
      25,
      ?FORALL(
         {Request, {Cookie, Path, Expires, MaxAge},
          {{_, Status, Reason}, _, _} = Response},
         {valid_http_request(), cookie_max_age(), valid_http_response()},
         begin
             ResponseBin = build_response(Response, [Cookie]),
             ValidationFun = validate_cookie(ResponseBin, fun check_cookie_deleted/3,
                                             [Expires, Cookie]),
             WaitTime = expiration_time(Expires, MaxAge),
             Responses =
                 send_cookie_requests(Host, Ssl, Family, ValidationFun, Path,
                                      Request, [<<"first">>, <<"second">>],
                                      WaitTime),
             check_responses(Status, Reason, Responses)
         end
        )
     ).

prop_http_request_expires(Host, Family, Ssl) ->
    ?FORALL(
       {Request, {Cookie, Path, Expires},
        {{_, Status, Reason}, _, _} = Response},
       {valid_http_request(), cookie_expires(), valid_http_response()},
       begin
           ResponseBin = build_response(Response, [Cookie]),
           ValidationFun = validate_cookie(ResponseBin, fun check_cookie_deleted/3,
                                           [Expires, Cookie]),
           Responses =
               send_cookie_requests(Host, Ssl, Family, ValidationFun, Path,
                                    Request, [<<"first">>, <<"second">>], 0),
           check_responses(Status, Reason, Responses)
       end
      ).
%%==============================================================================
%% Internal functions
%%==============================================================================
validate_msg({{_Method, _Uri, _Version}, SentHeaders, SentBody}) ->
    fun(Module, Socket, _Request, GotHeaders, GotBody) when SentBody == GotBody ->
	    case validate_headers(SentBody, GotHeaders, SentHeaders) of
		true ->
		    Module:send(Socket, ?TWO_OK);
		false ->
		    Module:send(Socket, ?FOUR_BAD_REQUEST)
	    end;
       (Module, Socket, _Request, _GotHeaders, _) ->
	    Module:send(Socket, ?FOUR_BAD_REQUEST) 
    end.

validate_cookie(Response, Fun, Params) ->
    fun(Module, Socket, _Request, Headers, _Body) ->
            case proplists:get_value("Test", Headers) of
                "first" ->
                    Module:send(Socket, Response);
                "second" ->
                    case erlang:apply(Fun, [Headers | Params]) of
                        true ->
                            Module:send(Socket, ?TWO_OK);
                        false ->
                            Module:send(Socket, ?FOUR_BAD_REQUEST)
                    end
            end
    end.    

validate_cookie_supersede(FirstResponse, SecondResponse, Supersede, FirstPair,
                          SecondPair) ->
    fun(Module, Socket, _Request, Headers, _Body) ->
           case proplists:get_value("Test", Headers) of
                "first" ->
                    Module:send(Socket, FirstResponse);
                "second" ->
                    Module:send(Socket, SecondResponse);
                "third" ->
                    case check_cookie_supersede(Headers, Supersede, FirstPair, SecondPair) of
                        true ->
                            Module:send(Socket, ?TWO_OK);
                        false ->
                            Module:send(Socket, ?FOUR_BAD_REQUEST)
                    end
            end
    end.

build_cookie(N, V) ->
    binary_to_list(<<N/binary,"=",V/binary>>).

check_cookie_deleted(Headers, true, _) ->
    undefined == proplists:get_value("Cookie", Headers);
check_cookie_deleted(Headers, false, {_, {{N, V}, _}}) ->
    build_cookie(N, V) == proplists:get_value("Cookie", Headers).  

%% http://tools.ietf.org/search/rfc6265#section-4.1.2
check_cookie_supersede(Headers, true, {Name, _, _, _}, {_, NewValue, _, _}) ->
    build_cookie(Name, NewValue) == proplists:get_value("Cookie", Headers);
check_cookie_supersede(Headers, false, {Name, Value, Path, _},
                       {NewName, NewValue, Path, _}) ->
    lists:sort([build_cookie(Name, Value), build_cookie(NewName, NewValue)])
        == lists:sort(string:tokens(proplists:get_value("Cookie", Headers), "; "));
check_cookie_supersede(Headers, false, {Name, Value, _, _}, _) ->
    build_cookie(Name, Value)
        == proplists:get_value("Cookie", Headers).

verify_host(GotHeaders, SentHeaders) ->
    %% Host must be added by the client if it is not part of the headers list
    %% http://tools.ietf.org/html/rfc2616#section-14.23
    Key = "host",
    case lists:keytake(Key, 1, GotHeaders) of
	{value, {_, Value}, NewGotHeaders} ->
	    case lists:keytake(Key, 1, SentHeaders) of
		%% The user sent the 'Host' header, value must match
		{value, {_, Value}, NewSentHeaders} ->
		    {NewGotHeaders, NewSentHeaders};
		false ->
		    {NewGotHeaders, SentHeaders};
		_  ->
		    false
	    end;
	false ->
	    false
    end.

verify_content_length(Body, GotHeaders, SentHeaders) ->
    %% Must be updated when the code supports transfer-encoding
    %% http://tools.ietf.org/html/rfc2616#section-14.13
    Key = "content-length",
    ContentLength = iolist_size(Body),
    {NewGotHeaders, GotContentLength}
	= case lists:keytake(Key, 1, GotHeaders) of
	      {value, {_, Value}, H} ->
		  {H, list_to_integer(Value)};
	      false ->
		  {GotHeaders, 0}
	  end,
    case ContentLength == GotContentLength of
	true ->
	    {NewGotHeaders, lists:keydelete(Key, 1, SentHeaders)};
	false ->
	    false
    end.

validate_headers(Body, GotHeaders, SentHeaders) ->
    CleanGotHeaders = lists:keysort(1, [{string:to_lower(K), V}
					|| {K, V} <- GotHeaders]),
    CleanSentHeaders = lists:keysort(1, [{string:to_lower(binary_to_list(K)),
					  binary_to_list(V)}
					 || {K, V} <- SentHeaders]),
    case verify_host(CleanGotHeaders, CleanSentHeaders) of
	false ->
	    false;
	{GotHeaders1, Headers1} ->
	    case verify_content_length(Body, GotHeaders1, Headers1) of
		false ->
		    false;
		{GotHeaders2, Headers2} ->
		    GotHeaders2 == Headers2
	    end
    end.

reply_msg(Msg) ->
    fun(Module, Socket, _Request, _Headers, _Body) ->
	    Module:send(Socket, Msg)
    end.

reply_msg_and_check(Id, Msg) ->
    Parent = self(),
    fun(Module, Socket, _Request, _Headers, _Body) ->
	    Module:send(Socket, Msg),
	    case Module:recv(Socket, 0) of
		{error, closed} ->
		    Parent ! {Id, closed};
		_ ->
		    ok
	    end
    end.

reply_and_close_msg(Msg) ->
    fun(Module, Socket, _Request, _Headers, _Body) ->
	    Module:send(Socket, Msg),
	    Module:close(Socket)
    end.

must_close(Headers, Connection) ->
    case proplists:get_value(<<"Connection">>, Headers) of
	<<"close">> ->
	    true;
	_ ->
	    case Connection of
		<<"close">> ->
		    true;
		_ ->
		    false
	    end
    end.

select_module(Ssl) ->
    case Ssl of
	true ->
	    ssl;
	false ->
	    gen_tcp
    end.

encode_path(Path) ->
    case lists:split(length(Path) - 1, Path) of
        {P, ["/"]} ->
            list_to_binary(["/", string:join(P, "/"), "/"]);
        _ ->
            list_to_binary(["/", string:join(Path, "/")])
    end.

build_response({StatusLine, Headers, Body}, Cookies) ->
    http_eqc_encoding:build_valid_response(
      StatusLine,
      http_eqc_encoding:add_content_length(Headers, Body),
      Cookies, Body).

send_cookie_requests(Host, Ssl, Family, ValidationFun, Path,
                     {{Method, _Uri, _Version}, Headers, Body},
                     RequestTags, WaitTime) ->
    Module = select_module(Ssl),
    {ok, Listener, LS, Port} =
        webserver:start(Module, [ValidationFun], Family),
    {ok, Client} = fusco:start({Host, Port, Ssl}, [{use_cookies, true}]),
    %% Use header "test" to distinguish requests in the server side
    Responses = lists:map(
                  fun(Header) ->
                          {ok, {Response, _, _, _, _}}
                              = fusco:request(Client, Path, Method,
                                              [{<<"test">>, Header} | Headers],
                                              Body, 10000),
                          timer:sleep(WaitTime),
                          Response
                  end, RequestTags),
    ok = fusco:disconnect(Client),
    webserver:stop(Module, Listener, LS),
    Responses.

expiration_time(Expires, MaxAge) ->
    case Expires of
        true ->
            MaxAge*1000;
        false ->
            0
    end.

check_responses(Status, Reason, [First, Second]) ->
    Expected = {Status, Reason},
    ?WHENFAIL(io:format("First: ~p~nExpected: ~p~nSecond: ~p~n",
                        [First, Expected, Second]),
              {First, Second} == {Expected, {<<"200">>, <<"OK">>}}
             );
check_responses(Status, Reason, [First, Second, Third]) ->
    Expected = {Status, Reason},
    ?WHENFAIL(io:format("First: ~p~nSecond: ~p~nExpected: ~p~nThird ~p~n",
                        [First, Second, Expected, Third]),
              {First, Second, Third}
              == {Expected, Expected, {<<"200">>, <<"OK">>}}
             ).

diff(A, A) ->
    eq;
diff(A, B) when A < B ->
    lt;
diff(_, _) ->
    gt.

is_past(DateTime) ->
    {Date, {H, M, _}} = http_eqc_encoding:expires_datetime(DateTime),
    case diff(Date, date()) of
        lt ->
            true;
        gt ->
            false;
        eq ->
            {HH, MM, _} = time(),
            %% Set 30 min margin, safe side
            case diff((HH*60+MM) - (H*60+M), 30) of
                gt ->
                    true;
                eq ->
                    true;
                lt ->
                    false
            end
    end.

is_future(DateTime) ->
    {Date, {H, M, _}} = http_eqc_encoding:expires_datetime(DateTime),
    case diff(Date, date()) of
        gt ->
            true;
        lt ->
            false;
        eq ->
            {HH, MM, _} = time(),
            %% Set 30 min margin, safe side
            case diff((H*60+M) - (HH*60+MM), 30) of
                gt ->
                    true;
                eq ->
                    true;
                lt ->
                    false
            end
    end.
