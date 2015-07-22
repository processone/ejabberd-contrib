%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(fusco_protocol_SUITE).
-copyright("2013, Erlang Solutions Ltd.").

-compile(export_all).

all() ->
    [prop_http_response_close_connection,
     prop_http_response_keep_alive,
     prop_chunked_http_response_keep_alive].

%%==============================================================================
%% Test cases
%%==============================================================================
prop_http_response_close_connection(_) ->
    do_prop(prop_http_response_close_connection).

prop_http_response_keep_alive(_) ->
    do_prop(prop_http_response_keep_alive).

prop_chunked_http_response_keep_alive(_) ->
    do_prop(prop_chunked_http_response_keep_alive).

%%==============================================================================
%% Internal functions
%%==============================================================================
do_prop(Case) ->
    case eqc:counterexample(erlang:apply(fusco_protocol_eqc, Case, [])) of
        true ->
            true;
        Value ->
            exit(Value)
    end.
