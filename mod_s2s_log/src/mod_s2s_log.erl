%%%----------------------------------------------------------------------
%%% File    : mod_s2s_log.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Log all s2s connections in a file
%%% Created :  14 Mar 2008 by Mickael Remond <mremond@process-one.net>
%%% Usage   : Add the following line in modules section of ejabberd.cfg:
%%%              {mod_s2s_log, [{filename, "/path/to/s2s.log"}]}
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
-module(mod_s2s_log).
-author('mremond@process-one.net').

%% TODO: Implement multi vhosts support
%% TODO: Support reopen log event

-behaviour(gen_mod).

%% API:
-export([start/2,
         init/1,
	 stop/1]).
%% Hooks:
-export([reopen_log/0,
	 s2s_connect/2]).

-include("ejabberd.hrl").

-define(PROCNAME, ?MODULE).
-define(DEFAULT_FILENAME, <<"s2s.log">>).
-define(FILE_OPTS, [append,raw]).

-record(config, {filename=?DEFAULT_FILENAME, iodevice}).

%% For now we only support one log file for all vhosts.
start(_Host, Opts) ->
    %% ejabberd starts modules sequentially so we assume no race
    %% condition is possible here
    case whereis(?PROCNAME) of
	undefined ->
	    Filename = gen_mod:get_opt(filename, Opts, fun(V) -> V end, ?DEFAULT_FILENAME),
	    %% TODO: Both hooks will need Host parameter for vhost support
	    ejabberd_hooks:add(reopen_log_hook, ?MODULE, reopen_log, 55),
	    ejabberd_hooks:add(s2s_connect_hook, ?MODULE, s2s_connect, 55),
	    register(?PROCNAME,
		     spawn(?MODULE, init, [#config{filename=Filename}]));
	_ ->
	    ok
    end.

init(Config)->
    {ok, IOD} = file:open(Config#config.filename, ?FILE_OPTS),
    loop(Config#config{iodevice=IOD}).

loop(Config) ->
    receive
	{s2s_connect, MyServer, Server} ->
	    log_s2s_connection(Config#config.iodevice, MyServer, Server),
	    loop(Config);
	{reopen_log} ->
	    file:close(Config#config.iodevice),
	    {ok, IOD} = file:open(Config#config.filename, ?FILE_OPTS),
	    loop(Config#config{iodevice = IOD});
	stop ->
	    file:close(Config#config.iodevice),
	    exit(normal)
    end.

stop(Host) ->
    ejabberd_hooks:delete(s2s_connection, Host,
			  ?MODULE, s2s_connection, 55),
    ?PROCNAME ! stop,
    ok.

s2s_connect(MyServer, Server) ->
    ?PROCNAME ! {s2s_connect, MyServer, Server}.

reopen_log() ->
    ?PROCNAME ! {reopen_log}.


%% ---
%% Internal functions

log_s2s_connection(IODevice, MyServer, Server) ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    Date = io_lib:format(template(date), [Y, M, D, H, Min, S]),
    Record = [Date, "|", MyServer, "|", Server, "\n"],
    ok = file:write(IODevice, Record).

template(date) ->
    "~p-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w".
