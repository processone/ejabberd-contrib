%%%----------------------------------------------------------------------
%%% File    : mod_logxml.erl
%%% Author  : Badlop
%%% Purpose : Log XMPP packets to XML file
%%% Created : 
%%% Id      : 
%%%----------------------------------------------------------------------

-module(mod_logxml).
-author('badlop@ono.com').

-behaviour(gen_mod).

-export([start/2, init/7, stop/1,
	 send_packet/3, receive_packet/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_logxml).

%% -------------------
%% Module control
%% -------------------

start(Host, Opts) ->
    Logdir = gen_mod:get_opt(logdir, Opts, fun(A) -> A end, "/tmp/jabberlogs/"),

    Rd = gen_mod:get_opt(rotate_days, Opts, fun(A) -> A end, 1),
    Rf = case gen_mod:get_opt(rotate_megs, Opts, fun(A) -> A end, 10) of
	     no -> no;
	     Rf1 -> Rf1*1024*1024
	 end,
    Rp = case gen_mod:get_opt(rotate_kpackets, Opts, fun(A) -> A end, 10) of
	     no -> no;
	     Rp1 -> Rp1*1000
	 end,
    RotateO = {Rd, Rf, Rp},
    CheckRKP = gen_mod:get_opt(check_rotate_kpackets, Opts, fun(A) -> A end, 1),

    Timezone = gen_mod:get_opt(timezone, Opts, fun(A) -> A end, local),

    Orientation = gen_mod:get_opt(orientation, Opts, fun(A) -> A end, [send, recv]),
    Stanza = gen_mod:get_opt(stanza, Opts, fun(A) -> A end, [iq, message, presence, other]),
    Direction = gen_mod:get_opt(direction, Opts, fun(A) -> A end, [internal, vhosts, external]),
    FilterO = {
      {orientation, Orientation},
      {stanza, Stanza},
      {direction, Direction}},
    ShowIP = gen_mod:get_opt(show_ip, Opts, fun(A) -> A end, false),

    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, send_packet, 90),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, receive_packet, 90),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, init, [binary_to_list(Host), Logdir, RotateO, CheckRKP,
				   Timezone, ShowIP, FilterO])).

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, send_packet, 90),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, receive_packet, 90),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! stop,
    {wait, Proc}.

init(Host, Logdir, RotateO, CheckRKP, Timezone, ShowIP, FilterO) ->
    {IoDevice, Filename, Gregorian_day} = open_file(Logdir, Host, Timezone),
    loop(Host, IoDevice, Filename, Logdir, CheckRKP, RotateO, 0, Gregorian_day,
	 Timezone, ShowIP, FilterO).

%% -------------------
%% Main
%% -------------------

manage_rotate(Host, IoDevice, Filename, Logdir, RotateO, PacketC,
	      Gregorian_day_log, Timezone) ->
    {RO_days, RO_size, RO_packets} = RotateO,

    Rotate1 = case RO_packets of
		  no -> false;
		  PacketC -> true;
		  _ -> false
	      end,

    Filesize = filelib:file_size(Filename),
    Rotate2 = if 
		  RO_size == no -> false;
		  Filesize >= RO_size -> true;
		  true -> false
	      end,

    Gregorian_day_today = get_gregorian_day(),
    Rotate3 = if
		  RO_days == no -> false;
		  (Gregorian_day_today - Gregorian_day_log) >= RO_days -> 
		      true;
		  true -> false
	      end,

    case lists:any(fun(E) -> E end, [Rotate1, Rotate2, Rotate3]) of
	true -> 
	    {IoDevice2, Filename2, Gregorian_day2} =
		rotate_log(IoDevice, Logdir, Host, Timezone),
	    {IoDevice2, Filename2, Gregorian_day2, 0};
	false -> 
	    {IoDevice, Filename, Gregorian_day_log, PacketC+1}
    end.

filter(FilterO, E) ->
    {{orientation, OrientationO},{stanza, StanzaO},{direction, DirectionO}} =
	FilterO,
    {Orientation, From, To, Packet} = E,

    {xmlel, Stanza_str, _Attrs, _Els} = Packet,
    Stanza = list_to_atom(binary_to_list(Stanza_str)),

    Hosts_all = ejabberd_config:get_global_option(hosts, fun(A) -> A end),
    {Host_local, Host_remote} = case Orientation of
				    send -> {From#jid.lserver, To#jid.lserver};
				    recv -> {To#jid.lserver, From#jid.lserver}
				end,
    Direction = case Host_remote of
		    Host_local -> internal;
		    _ -> 
			case lists:member(Host_remote, Hosts_all) of
			    true -> vhosts;
			    false -> external
			end
		end,

    {lists:all(fun(O) -> O end, 
	       [lists:member(Orientation, OrientationO),
		lists:member(Stanza, StanzaO),
		lists:member(Direction, DirectionO)]),
     {Orientation, Stanza, Direction}}. 

loop(Host, IoDevice, Filename, Logdir, CheckRKP, RotateO, PacketC,
     Gregorian_day, Timezone, ShowIP, FilterO) ->
    receive
	{addlog, E} ->
	    {IoDevice3, Filename3, Gregorian_day3, PacketC3} =
		case filter(FilterO, E) of
		    {true, OSD} ->
			Div = calc_div(PacketC, CheckRKP),
			{IoDevice2, Filename2, Gregorian_day2, PacketC2} =
			    case Div==round(Div) of
				true ->
				    manage_rotate(Host, IoDevice, Filename,
						  Logdir, RotateO, PacketC,
						  Gregorian_day, Timezone);
				false ->
				    {IoDevice, Filename, Gregorian_day,
				     PacketC+1}
			    end,
			add_log(IoDevice2, Timezone, ShowIP, E, OSD),
			{IoDevice2, Filename2, Gregorian_day2, PacketC2};
		    _ ->
			{IoDevice, Filename, Gregorian_day, PacketC}
		end,
	    loop(Host, IoDevice3, Filename3, Logdir, CheckRKP, RotateO,
		 PacketC3, Gregorian_day3, Timezone, ShowIP, FilterO);
	stop ->
	    close_file(IoDevice),
	    ok;
	_ ->
	    loop(Host, IoDevice, Filename, Logdir, CheckRKP, RotateO, PacketC,
		 Gregorian_day, Timezone, ShowIP, FilterO)
    end.

send_packet(FromJID, ToJID, P) ->
    Host = FromJID#jid.lserver,
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! {addlog, {send, FromJID, ToJID, P}}.

receive_packet(_JID, From, To, P) ->
    Host = To#jid.lserver,
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! {addlog, {recv, From, To, P}}.

add_log(Io, Timezone, ShowIP, {Orientation, From, To, Packet}, _OSD) ->
    %%{Orientation, Stanza, Direction} = OSD, 
    LocalJID = case Orientation of
		   send -> From;
		   recv -> To
	       end,
    LocalIPS = case ShowIP of
		   true -> 
		       {UserIP, _Port} = ejabberd_sm:get_user_ip(
					   LocalJID#jid.user,
					   LocalJID#jid.server,
					   LocalJID#jid.resource),
		       io_lib:format("lip=\"~s\" ", [inet_parse:ntoa(UserIP)]);
		   false -> ""
	       end,
    TimestampISO = get_now_iso(Timezone),
    io:fwrite(Io, "<packet or=\"~p\" ljid=\"~s\" ~sts=\"~s\">~s</packet>~n",
	      [Orientation, jlib:jid_to_string(LocalJID), LocalIPS,
	       TimestampISO, binary_to_list(xml:element_to_binary(Packet))]).

%% -------------------
%% File
%% -------------------

open_file(Logdir, Host, Timezone) ->
    TimeStamp = get_now_iso(Timezone),
    Year = string:substr(TimeStamp, 1, 4),
    Month = string:substr(TimeStamp, 5, 2),
    Day = string:substr(TimeStamp, 7, 2),
    Hour = string:substr(TimeStamp, 10, 2),
    Min = string:substr(TimeStamp, 13, 2),
    Sec = string:substr(TimeStamp, 16, 2),
    S = "-",
    Logname = lists:flatten([Host,S,Year,S,Month,S,Day,S,Hour,S,Min,S,Sec,
			     ".xml"]),
    Filename = filename:join([Logdir, Logname]),

    Gregorian_day = get_gregorian_day(),

    %% Open file, create if it does not exist, create parent dirs if needed
    case file:read_file_info(Filename) of
	{ok, _} ->
	    {ok, IoDevice} = file:open(Filename, [append]);
	{error, enoent} ->
	    make_dir_rec(Logdir),
	    {ok, IoDevice} = file:open(Filename, [append]),
	    io:fwrite(IoDevice, "~s~n", ["<?xml version=\"1.0\"?>"]),
	    io:fwrite(IoDevice, "~s~n", ["<?xml-stylesheet href=\"xmpp.xsl\" type=\"text/xsl\"?>"]),
	    io:fwrite(IoDevice, "~s~n", ["<log>"])
    end,
    {IoDevice, Filename, Gregorian_day}.

close_file(IoDevice) ->
    io:fwrite(IoDevice, "~s~n", ["</log>"]),
    file:close(IoDevice).

rotate_log(IoDevice, Logdir, Host, Timezone) ->
    close_file(IoDevice),
    open_file(Logdir, Host, Timezone).

make_dir_rec(Dir) ->
    case file:read_file_info(Dir) of
	{ok, _} ->
	    ok;
	{error, enoent} ->
	    DirS = filename:split(Dir),
	    DirR = lists:sublist(DirS, length(DirS)-1),
	    make_dir_rec(filename:join(DirR)),
	    file:make_dir(Dir)
    end.

%% -------------------
%% Utils
%% -------------------

get_gregorian_day() -> calendar:date_to_gregorian_days(date()).

get_now_iso(Timezone) ->
    TimeStamp = case Timezone of
		    local -> calendar:now_to_local_time(now());
		    universal -> calendar:now_to_universal_time(now())
		end,
    binary_to_list(jlib:timestamp_to_iso(TimeStamp)).

calc_div(A, B) when is_integer(A) and is_integer(B) and B =/= 0 ->
    A/B;
calc_div(_A, _B) ->
    0.5. %% This ensures that no rotation is performed
