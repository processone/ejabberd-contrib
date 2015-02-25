%%%----------------------------------------------------------------------
%%% File    : mod_logsession.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Log session connections to file
%%% Created :  8 Jan 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2008   ProcessOne
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


-module(mod_logsession).
-author('badlop@process-one.net').

-behaviour(gen_mod).

-export([
	 start/2,
	 stop/1,
	 loop/3,
	 reopen_log/1,
	 failed_auth/4,
	 forbidden/1
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_commands.hrl").

-define(PROCNAME, ejabberd_logsession).

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------

start(Host, Opts) ->
    ejabberd_hooks:add(reopen_log_hook, Host, ?MODULE, reopen_log, 50),
    ejabberd_hooks:add(forbidden_session_hook, Host, ?MODULE, forbidden, 50),
    ejabberd_hooks:add(failed_auth_hook, Host, ?MODULE, failed_auth, 50),
    ejabberd_commands:register_commands(commands()),
    Filename1 = gen_mod:get_opt(
		  sessionlog, 
		  Opts, 
		  fun(S) -> S end,
		  "/tmp/ejabberd_logsession_@HOST@.log"),
    Filename = replace_host(Host, Filename1),
    File = open_file(Filename),
    register(get_process_name(Host), spawn(?MODULE, loop, [Filename, File, Host])).

stop(Host) ->
    ejabberd_hooks:delete(reopen_log_hook, Host, ?MODULE, reopen_log, 50),
    ejabberd_hooks:delete(forbidden_session_hook, Host, ?MODULE, forbidden, 50),
    ejabberd_commands:unregister_commands(commands()),
    Proc = get_process_name(Host),
    exit(whereis(Proc), stop),
    {wait, Proc}.

%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

reopen_log(Host) ->
    get_process_name(Host) ! reopenlog.

forbidden(JID) ->
    Host = JID#jid.lserver,
    get_process_name(Host) ! {log, forbidden, JID}.

failed_auth(AuthType, U, Host, IPPT) ->
    get_process_name(Host) ! {log, failed_auth, {AuthType, U, IPPT}}.

commands() ->
    [#ejabberd_commands{name = reopen_seslog, tags = [logs, server],
			desc = "Reopen mod_logsession log file",
			module = ?MODULE, function = reopen_log,
			args = [{host, string}],
			result = {res, rescode}}].

%%%----------------------------------------------------------------------
%%% LOOP
%%%----------------------------------------------------------------------

loop(Filename, File, Host) ->
    receive
	{log, Type, Data} ->
	    log(File, Host, Type, Data),
	    loop(Filename, File, Host);
	reopenlog ->
	    File2 = reopen_file(File, Filename),
	    loop(Filename, File2, Host);
	stop ->
	    close_file(File)
    end.

%%%----------------------------------------------------------------------
%%% UTILITIES
%%%----------------------------------------------------------------------

get_process_name(Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME).

replace_host(Host, Filename) ->
    re:replace(Filename, "@HOST@", binary_to_list(Host), [global, {return, list}]).

open_file(Filename) -> 
    {ok, File} = file:open(Filename, [append]),
    File.

close_file(File) -> 
    file:close(File).

reopen_file(File, Filename) -> 
    close_file(File),
    open_file(Filename).

log(File, Host, Type, Data) ->
    DateString = make_date(calendar:local_time()),
    MessageString = make_message(Host, Type, Data),
    io:format(File, "~s ~s~n", [DateString, MessageString]).

make_date(Date) ->
    {{Y, Mo, D}, {H, Mi, S}} = Date,
    %% Combined format:
    %%io_lib:format("[~p/~p/~p:~p:~p:~p]", [D, Mo, Y, H, Mi, S]).
    %% Erlang format:
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w", 
		  [Y, Mo, D, H, Mi, S]).

make_message(Host, Type, {AuthType, Username, {IPTuple, IPPort}}) ->
    String = get_string(Type),
    IPString = inet_parse:ntoa(IPTuple),
    io_lib:format(String, [AuthType, Username, Host, IPString, IPPort]);
make_message(_Host, Type, JID) ->
    String = get_string(Type),
    io_lib:format(String, [jlib:jid_to_string(JID)]).

get_string(failed_auth) -> "Failed ~p authentication for ~s@~s from ~s port ~p";
get_string(forbidden) -> "Forbidden session for ~s".
