%%%----------------------------------------------------------------------
%%% File    : mod_couch.erl
%%% Author  : Eric Cestari
%%% Purpose : Configures and starts ecouch client
%%% Created : 
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_couch).
-author('eric@ohmforce.com').
-vsn('0.2.0').

-behaviour(gen_mod).

-export([start/2, stop/1]).

start(Host, Opts) ->
	Server = gen_mod:get_opt(server, Opts, {"127.0.0.1", "5984"}),
	inets:start(),
	application:set_env(ecouch, Server, {}),
	application:start(ecouch).

stop(_Host) ->
	application:stop(ecouch).