%%%-------------------------------------------------------------------
%%% File    : mod_message_log.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Log one line per message transmission
%%% Created : 27 May 2014 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%-------------------------------------------------------------------

-module(mod_message_log).
-author('holger@zedat.fu-berlin.de').

-behaviour(gen_mod).
-behaviour(gen_server).

%% gen_mod/supervisor callbacks.
-export([start_link/1,
	 start/2,
	 stop/1,
	 mod_opt_type/1]).

%% gen_server callbacks.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% ejabberd_hooks callbacks.
-export([log_packet_send/4,
	 log_packet_receive/5,
	 log_packet_offline/3,
	 reopen_log/0]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(PROCNAME, ?MODULE).
-define(DEFAULT_FILENAME, <<"message.log">>).
-define(FILE_MODES, [append, raw]).

-record(state, {filename = ?DEFAULT_FILENAME :: binary(),
		iodevice                     :: io:device()}).

-type direction() :: incoming | outgoing | offline.
-type state() :: #state{}.

%% -------------------------------------------------------------------
%% gen_mod/supervisor callbacks.
%% -------------------------------------------------------------------

-spec start_link(gen_mod:opts()) -> {ok, pid()} | ignore | {error, _}.

start_link(Opts) ->
    gen_server:start_link({local, ?PROCNAME}, ?MODULE, Opts, []).

-spec start(binary(), gen_mod:opts()) -> {ok, _} | {ok, _, _} | {error, _}.

start(Host, Opts) ->
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       log_packet_send, 42),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       log_packet_receive, 42),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE,
		       log_packet_offline, 42),
    Spec = {
	?PROCNAME,
	{?MODULE, start_link, [Opts]},
	permanent,
	3000,
	worker,
	[?MODULE]
    },
    supervisor:start_child(ejabberd_sup, Spec).

-spec stop(binary()) -> ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  log_packet_send, 42),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  log_packet_receive, 42),
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE,
			  log_packet_offline, 42),
    case supervisor:terminate_child(ejabberd_sup, ?PROCNAME) of
      ok ->
	  ok = supervisor:delete_child(ejabberd_sup, ?PROCNAME);
      {error, not_found} ->
	  ok % We just run one process per node.
    end.

-spec mod_opt_type(atom()) -> fun((term()) -> term()) | [atom()].

mod_opt_type(filename) ->
    fun iolist_to_binary/1;
mod_opt_type(_) ->
    [filename].

%% -------------------------------------------------------------------
%% gen_server callbacks.
%% -------------------------------------------------------------------

-spec init(gen_mod:opts()) -> {ok, state()}.

init(Opts) ->
    process_flag(trap_exit, true),
    ejabberd_hooks:add(reopen_log_hook, ?MODULE, reopen_log, 42),
    Filename = gen_mod:get_opt(filename, Opts, fun iolist_to_binary/1,
			       ?DEFAULT_FILENAME),
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

-spec log_packet_send(xmlel(), term(), jid(), jid()) -> xmlel().

log_packet_send(Packet, _C2SState, From, To) ->
    log_packet(outgoing, From, To, Packet),
    Packet.

-spec log_packet_receive(xmlel(), term(), jid(), jid(), jid()) -> xmlel().

log_packet_receive(Packet, _C2SState, JID, From, _To) ->
    log_packet(incoming, From, JID, Packet),
    Packet.

-spec log_packet_offline(jid(), jid(), xmlel()) -> any().

log_packet_offline(From, To, Packet) ->
    log_packet(offline, From, To, Packet).

-spec reopen_log() -> any().

reopen_log() ->
    gen_server:cast(?PROCNAME, reopen_log).

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

-spec log_packet(direction(), jid(), jid(), xmlel()) -> any().

log_packet(Direction, From, To, #xmlel{name = <<"message">>} = Packet) ->
    case fxml:get_subtag(Packet, <<"body">>) of
      #xmlel{children = Body} when length(Body) > 0 ->
	  Type = get_message_type(Packet),
	  gen_server:cast(?PROCNAME, {message, Direction, From, To, Type});
      _ ->
	  case is_carbon(Packet) of
	    {true, OrigDirection} ->
		gen_server:cast(?PROCNAME, {message, OrigDirection, From, To,
					    carbon});
	    false ->
		ok
	  end
    end;
log_packet(_Direction, _From, _To, _Packet) ->
    ok.

-spec get_message_type(xmlel()) -> binary().

get_message_type(#xmlel{attrs = Attrs}) ->
    case fxml:get_attr_s(<<"type">>, Attrs) of
      <<"">> ->
	  <<"normal">>;
      Type ->
	  Type
    end.

-spec is_carbon(xmlel()) -> {true, direction()} | false.

is_carbon(Packet) ->
    {Direction, SubTag} = case {fxml:get_subtag(Packet, <<"sent">>),
				fxml:get_subtag(Packet, <<"received">>)} of
			    {false, false} ->
				{false, false};
			    {false, Tag} ->
				{incoming, Tag};
			    {Tag, _} ->
				{outgoing, Tag}
			  end,
    F = fun(_, false) ->
	       false;
	   (Name, Tag) ->
	       fxml:get_subtag(Tag, Name)
	end,
    case lists:foldl(F, SubTag, [<<"forwarded">>, <<"message">>, <<"body">>]) of
      #xmlel{children = Body} when length(Body) > 0 ->
	  {true, Direction};
      _ ->
	  false
    end.

-spec write_log(io:device(), direction(), jid(), jid(), binary()) -> ok.

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
