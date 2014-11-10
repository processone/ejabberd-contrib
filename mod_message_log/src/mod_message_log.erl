%%%-------------------------------------------------------------------
%%% File    : mod_message_log.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Log one line per message transmission
%%% Created : 27 May 2014 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%-------------------------------------------------------------------

-module(mod_message_log).
-author('holger@zedat.fu-berlin.de').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 init/1,
	 perform_upgrade/1]).

-export([log_packet_send/3,
	 log_packet_receive/4,
	 log_packet_offline/3,
	 reopen_log/0]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(PROCNAME, ?MODULE).
-define(DEFAULT_FILENAME, <<"message.log">>).
-define(FILE_MODES, [append, raw]).

-record(config, {filename = ?DEFAULT_FILENAME, iodevice}).

start(Host, Opts) ->
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       log_packet_send, 42),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       log_packet_receive, 42),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE,
		       log_packet_offline, 42),
    case whereis(?PROCNAME) of
      undefined ->
	  ejabberd_hooks:add(reopen_log_hook, ?MODULE, reopen_log, 42),
	  Filename = gen_mod:get_opt(filename, Opts, fun(V) -> V end,
				     ?DEFAULT_FILENAME),
	  register(?PROCNAME, spawn(?MODULE, init,
				    [#config{filename = Filename}])),
	  ok;
      _ ->
	  ok
    end.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  log_packet_send, 42),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  log_packet_receive, 42),
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE,
			  log_packet_offline, 42),
    case whereis(?PROCNAME) of
      undefined ->
	  ok;
      _ ->
	  ejabberd_hooks:delete(reopen_log_hook, ?MODULE, reopen_log, 42),
	  gen_mod:get_module_proc(Host, ?PROCNAME) ! stop,
	  ok
    end.

init(Config) ->
    {ok, IoDevice} = file:open(Config#config.filename, ?FILE_MODES),
    loop(Config#config{iodevice = IoDevice}).

log_packet_send(From, To, Packet) ->
    log_packet(outgoing, From, To, Packet).

log_packet_receive(JID, From, _To, Packet) ->
    log_packet(incoming, From, JID, Packet).

log_packet_offline(From, To, Packet) ->
    log_packet(offline, From, To, Packet).

reopen_log() ->
    ?PROCNAME ! reopen.

%% Internal functions.

log_packet(Direction, From, To, #xmlel{name = <<"message">>} = Packet) ->
    case xml:get_subtag(Packet, <<"body">>) of
      #xmlel{children = Body} when length(Body) > 0 ->
	  Type = get_message_type(Packet),
	  ?PROCNAME ! {message, Direction, From, To, Type};
      _ ->
	  case is_carbon(Packet) of
	    {true, OrigDirection} ->
		?PROCNAME ! {message, OrigDirection, From, To, carbon};
	    false ->
		ok
	  end
    end;
log_packet(_Direction, _From, _To, _Packet) ->
    ok.

get_message_type(#xmlel{attrs = Attrs}) ->
    case xml:get_attr_s(<<"type">>, Attrs) of
      <<"">> ->
	  <<"normal">>;
      Type ->
	  Type
    end.

is_carbon(Packet) ->
    {Direction, SubTag} = case {xml:get_subtag(Packet, <<"sent">>),
				xml:get_subtag(Packet, <<"received">>)} of
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
	       xml:get_subtag(Tag, Name)
	end,
    case lists:foldl(F, SubTag, [<<"forwarded">>, <<"message">>, <<"body">>]) of
      #xmlel{children = Body} when length(Body) > 0 ->
	  {true, Direction};
      _ ->
	  false
    end.

loop(Config) ->
    receive
      {message, Direction, From, To, Type} ->
	  write_log(Config#config.iodevice, Direction, From, To, Type),
	  loop(Config);
      reopen ->
	  file:close(Config#config.iodevice),
	  {ok, IoDevice} = file:open(Config#config.filename, ?FILE_MODES),
	  loop(Config#config{iodevice = IoDevice});
      upgrade ->
	  mod_message_log:perform_upgrade(Config);
      stop ->
	  exit(normal)
    end.

write_log(IoDevice, Direction, From, To, Type) ->
    Date = format_date(calendar:local_time()),
    Record = io_lib:format("~s [~s, ~s] ~s -> ~s~n",
			   [Date, Direction, Type,
			    jlib:jid_to_string(From),
			    jlib:jid_to_string(To)]),
    ok = file:write(IoDevice, [Record]).

format_date({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    Format = "~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
    io_lib:format(Format, [Year, Month, Day, Hour, Minute, Second]).

perform_upgrade(Config) ->
    loop(Config).
