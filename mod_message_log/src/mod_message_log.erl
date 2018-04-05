%%%----------------------------------------------------------------------
%%% File    : mod_message_log.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Log one line per message transmission
%%% Created : 27 May 2014 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2018   ProcessOne
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

-module(mod_message_log).
-author('holger@zedat.fu-berlin.de').

-behaviour(gen_mod).
-behaviour(gen_server).

%% gen_mod callbacks.
-export([start/2,
	 stop/1,
	 mod_opt_type/1,
	 mod_options/1,
	 depends/2]).

%% gen_server callbacks.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% ejabberd_hooks callbacks.
-export([log_packet_send/1,
	 log_packet_receive/1,
	 log_packet_offline/1,
	 reopen_log/0]).

-include("xmpp.hrl").

-define(DEFAULT_FILENAME, <<"message.log">>).
-define(FILE_MODES, [append, raw]).

-record(state, {filename = ?DEFAULT_FILENAME :: binary(),
		iodevice                     :: io:device()}).

-type direction() :: incoming | outgoing | offline.
-type state() :: #state{}.
-type c2s_state() :: ejabberd_c2s:state().
-type c2s_hook_acc() :: {stanza() | drop, c2s_state()}.

%% -------------------------------------------------------------------
%% gen_mod callbacks.
%% -------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> {ok, _} | {ok, _, _} | {error, _}.
start(Host, Opts) ->
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       log_packet_send, 42),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       log_packet_receive, 42),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE,
		       log_packet_offline, 42),
    case gen_mod:start_child(?MODULE, global, Opts) of
	{ok, Ref} ->
	    {ok, Ref};
	{error, {already_started, Ref}} ->
	    {ok, Ref};
	{error, Reason} ->
	    {error, Reason}
    end.

-spec stop(binary()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  log_packet_send, 42),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  log_packet_receive, 42),
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE,
			  log_packet_offline, 42),
    gen_mod:stop_child(?MODULE, global),
    ok.

-spec mod_opt_type(atom()) -> fun((term()) -> term()).
mod_opt_type(filename) ->
    fun iolist_to_binary/1.

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(_Host) ->
    [{filename, ?DEFAULT_FILENAME}].

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

%% -------------------------------------------------------------------
%% gen_server callbacks.
%% -------------------------------------------------------------------
-spec init(list()) -> {ok, state()}.
init([_Host, Opts]) ->
    process_flag(trap_exit, true),
    ejabberd_hooks:add(reopen_log_hook, ?MODULE, reopen_log, 42),
    Filename = gen_mod:get_opt(filename, Opts),
    {ok, IoDevice} = file:open(Filename, ?FILE_MODES),
    {ok, #state{filename = Filename, iodevice = IoDevice}}.

-spec handle_call(_, {pid(), _}, state()) -> {noreply, state()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast({message, Direction, From, To, Type}, #state{iodevice = IoDevice} =
	    State) ->
    write_log(IoDevice, Direction, From, To, Type),
    {noreply, State};
handle_cast(reopen_log, #state{filename = Filename, iodevice = IoDevice} =
	    State) ->
    ok = file:close(IoDevice),
    {ok, NewIoDevice} = file:open(Filename, ?FILE_MODES),
    {noreply, State#state{iodevice = NewIoDevice}};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(timeout | _, state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, _} | _, _) -> any().
terminate(_Reason, State) ->
    ejabberd_hooks:delete(reopen_log_hook, ?MODULE, reopen_log, 42),
    ok = file:close(State#state.iodevice).

-spec code_change({down, _} | _, state(), _) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -------------------------------------------------------------------
%% ejabberd_hooks callbacks.
%% -------------------------------------------------------------------
-spec log_packet_send(c2s_hook_acc()) -> c2s_hook_acc().
log_packet_send({#message{} = Msg, _C2SState} = Acc) ->
    log_packet(outgoing, Msg),
    Acc;
log_packet_send({_Stanza, _C2SState} = Acc) ->
    Acc.

-spec log_packet_receive(c2s_hook_acc()) -> c2s_hook_acc().
log_packet_receive({#message{} = Msg, _C2SState} = Acc) ->
    log_packet(incoming, Msg),
    Acc;
log_packet_receive({_Stanza, _C2SState} = Acc) ->
    Acc.

-spec log_packet_offline({any(), message()}) -> {any(), message()}.
log_packet_offline({_Action, Msg} = Acc) ->
    log_packet(offline, Msg),
    Acc.

-spec reopen_log() -> any().
reopen_log() ->
    Proc = gen_mod:get_module_proc(global, ?MODULE),
    gen_server:cast(Proc, reopen_log).

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------
-spec log_packet(direction(), message()) -> any().
log_packet(Direction, #message{from = From, to = To, type = Type} = Msg) ->
    case should_log(Msg) of
	true ->
	    {Type1, Direction1} = case is_carbon(Msg) of
				      {true, Direction0} ->
					  {carbon, Direction0};
				      false ->
					  {Type, Direction}
				  end,
	    Proc = gen_mod:get_module_proc(global, ?MODULE),
	    gen_server:cast(Proc, {message, Direction1, From, To, Type1});
	false ->
	    ok
    end.

-spec is_carbon(message()) -> {true, direction()} | false.
is_carbon(#message{meta = #{carbon_copy := true}} = Msg) ->
    case xmpp:has_subtag(Msg, #carbons_sent{}) of
	true ->
	    {true, outgoing};
	false ->
	    {true, incoming}
    end;
is_carbon(_Msg) ->
    false.

-spec should_log(message()) -> boolean().
should_log(#message{meta = #{carbon_copy := true}} = Msg) ->
    should_log(xmpp_util:unwrap_carbon(Msg));
should_log(#message{type = error}) ->
    false;
should_log(#message{body = Body, sub_els = SubEls}) ->
    xmpp:get_text(Body) /= <<>>
	orelse lists:any(fun(#xmlel{name = <<"encrypted">>}) -> true;
			    (_) -> false
			 end, SubEls).

-spec write_log(io:device(), direction(), jid(), jid(),
		message_type() | offline | carbon) -> ok.
write_log(IoDevice, Direction, From, To, Type) ->
    Date = format_date(calendar:local_time()),
    Record = io_lib:format("~s [~s, ~s] ~s -> ~s~n",
			   [Date, Direction, Type,
			    jlib:jid_to_string(From),
			    jlib:jid_to_string(To)]),
    ok = file:write(IoDevice, [Record]).

-spec format_date(calendar:datetime()) -> io_lib:chars().
format_date({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    Format = "~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
    io_lib:format(Format, [Year, Month, Day, Hour, Minute, Second]).
