%%%----------------------------------------------------------------------
%%% File    : mod_logsession.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Log session connections to file
%%% Created :  8 Jan 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2008-2020   ProcessOne
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

-export([start/2, stop/1, depends/2, mod_options/1, mod_opt_type/1, mod_doc/0, mod_status/0]).
-export([loop/3,
	 reopen_log/0,
	 failed_auth/3,
	 forbidden/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_commands.hrl").

-define(PROCNAME, ejabberd_logsession).

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------

start(Host, Opts) ->
    ejabberd_hooks:add(reopen_log_hook, Host, ?MODULE, reopen_log, 50),
    ejabberd_hooks:add(forbidden_session_hook, Host, ?MODULE, forbidden, 50),
    ejabberd_hooks:add(c2s_auth_result, Host, ?MODULE, failed_auth, 50),
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:register_commands(?MODULE, commands());
        true ->
            ok
    end,
    Filename1 = case gen_mod:get_opt(sessionlog, Opts) of
                    auto ->
                        filename:join(filename:dirname(ejabberd_logger:get_log_path()),
                                      "session_@HOST@.log");
                    SL ->
                        SL
                end,
    Filename = replace_host(Host, Filename1),
    File = open_file(Filename),
    register(get_process_name(Host), spawn(?MODULE, loop, [Filename, File, Host])),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(reopen_log_hook, Host, ?MODULE, reopen_log, 50),
    ejabberd_hooks:delete(forbidden_session_hook, Host, ?MODULE, forbidden, 50),
    ejabberd_hooks:delete(c2s_auth_result, Host, ?MODULE, failed_auth, 50),
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:unregister_commands(commands());
        true ->
            ok
    end,
    Proc = get_process_name(Host),
    exit(whereis(Proc), stop),
    {wait, Proc}.

depends(_Host, _Opts) ->
    [].

mod_opt_type(sessionlog) ->
    econf:either(auto, econf:string()).

mod_options(_Host) ->
    [{sessionlog, auto}].

mod_doc() -> #{}.

mod_status() ->
    Host = ejabberd_config:get_myname(),
    Pid = get_process_name(Host),
    Pid ! {get_filename, self()},
    Filename = receive
	       {filename, F} ->
		   F
	   end,
    io_lib:format("Logging ~p to: ~s", [binary_to_list(Host), Filename]).

%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

reopen_log() ->
    lists:foreach(
      fun(Host) ->
              gen_server:cast(get_process_name(Host), reopenlog)
      end, ejabberd_option:hosts()).

forbidden(JID) ->
    Host = JID#jid.lserver,
    get_process_name(Host) ! {log, {forbidden, JID}}.

failed_auth(State, true, _) ->
    State;
failed_auth(#{lserver := Host, ip := IPPT} = State, {false, Reason}, U) ->
    get_process_name(Host) ! {log, {failed_auth, U, IPPT, Reason}},
    State.

commands() ->
    [#ejabberd_commands{name = reopen_seslog, tags = [logs, server],
			desc = "Reopen mod_logsession log files",
			module = ?MODULE, function = reopen_log,
			args = [],
			result = {res, rescode}}].

%%%----------------------------------------------------------------------
%%% LOOP
%%%----------------------------------------------------------------------

loop(Filename, File, Host) ->
    receive
	{log, Data} ->
	    log(File, Host, Data),
	    loop(Filename, File, Host);
	reopenlog ->
	    File2 = reopen_file(File, Filename),
	    loop(Filename, File2, Host);
        {get_filename, Pid} ->
	    Pid ! {filename, Filename},
	    loop(Filename, File, Host);
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

log(File, Host, Data) ->
    DateString = make_date(calendar:local_time()),
    MessageString = make_message(Host, Data),
    io:format(File, "~s ~s~n", [DateString, MessageString]).

make_date(Date) ->
    {{Y, Mo, D}, {H, Mi, S}} = Date,
    %% Combined format:
    %%io_lib:format("[~p/~p/~p:~p:~p:~p]", [D, Mo, Y, H, Mi, S]).
    %% Erlang format:
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w", 
		  [Y, Mo, D, H, Mi, S]).

make_message(Host, {failed_auth, Username, {IPTuple, IPPort}, Reason}) ->
    IPString = inet_parse:ntoa(IPTuple),
    io_lib:format("Failed authentication for ~s@~s from ~s port ~p: ~s",
	[Username, Host, IPString, IPPort, Reason]);
make_message(_Host, {forbidden, JID}) ->
    io_lib:format("Forbidden session for ~s", [jid:encode(JID)]).
