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

-export([start/2, init/6, stop/1,
	 send_packet/1, receive_packet/1,
	 mod_opt_type/1, mod_options/1, depends/2, mod_doc/0]).

-include_lib("xmpp/include/xmpp.hrl").

-define(PROCNAME, ejabberd_mod_logxml).

%% -------------------
%% Module control
%% -------------------

start(Host, Opts) ->
    Logdir = gen_mod:get_opt(logdir, Opts),

    Rd = case gen_mod:get_opt(rotate_days, Opts) of
	     0 -> no;
	     Rd1 -> Rd1
	 end,
    Rf = case gen_mod:get_opt(rotate_megs, Opts) of
	     0 -> no;
	     Rf1 -> Rf1*1024*1024
	 end,
    Rp = case gen_mod:get_opt(rotate_kpackets, Opts) of
	     0 -> no;
	     Rp1 -> Rp1*1000
	 end,
    RotateO = {Rd, Rf, Rp},
    CheckRKP = gen_mod:get_opt(check_rotate_kpackets, Opts),
    Orientation = gen_mod:get_opt(orientation, Opts),
    Stanza = gen_mod:get_opt(stanza, Opts),
    Direction = gen_mod:get_opt(direction, Opts),
    FilterO = {
      {orientation, Orientation},
      {stanza, Stanza},
      {direction, Direction}},
    ShowIP = gen_mod:get_opt(show_ip, Opts),

    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, send_packet, 90),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, receive_packet, 90),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, init, [binary_to_list(Host), Logdir, RotateO, CheckRKP,
				   ShowIP, FilterO])),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, send_packet, 90),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, receive_packet, 90),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! stop,
    {wait, Proc}.

init(Host, Logdir, RotateO, CheckRKP, ShowIP, FilterO) ->
    {IoDevice, Filename, Gregorian_day} = open_file(Logdir, Host),
    loop(Host, IoDevice, Filename, Logdir, CheckRKP, RotateO, 0, Gregorian_day,
	 ShowIP, FilterO).

depends(_Host, _Opts) ->
    [].

mod_doc() -> #{}.

%% -------------------
%% Main
%% -------------------

manage_rotate(Host, IoDevice, Filename, Logdir, RotateO, PacketC,
	      Gregorian_day_log) ->
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
		rotate_log(IoDevice, Logdir, Host),
	    {IoDevice2, Filename2, Gregorian_day2, 0};
	false -> 
	    {IoDevice, Filename, Gregorian_day_log, PacketC+1}
    end.

filter(FilterO, E) ->
    {{orientation, OrientationO},{stanza, StanzaO},{direction, DirectionO}} =
	FilterO,
    {Orientation, From, To, Packet} = E,
    Stanza = element(1, Packet),
    Hosts_all = ejabberd_config:get_option(hosts),
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
     Gregorian_day, ShowIP, FilterO) ->
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
						  Gregorian_day);
				false ->
				    {IoDevice, Filename, Gregorian_day,
				     PacketC+1}
			    end,
			add_log(IoDevice2, ShowIP, E, OSD),
			{IoDevice2, Filename2, Gregorian_day2, PacketC2};
		    _ ->
			{IoDevice, Filename, Gregorian_day, PacketC}
		end,
	    loop(Host, IoDevice3, Filename3, Logdir, CheckRKP, RotateO,
		 PacketC3, Gregorian_day3, ShowIP, FilterO);
	stop ->
	    close_file(IoDevice),
	    ok;
	_ ->
	    loop(Host, IoDevice, Filename, Logdir, CheckRKP, RotateO, PacketC,
		 Gregorian_day, ShowIP, FilterO)
    end.

send_packet({P, State}) ->
    {FromJID, ToJID} = get_from_to(P),
    Host = FromJID#jid.lserver,
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! {addlog, {send, FromJID, ToJID, P}},
    {P, State}.

receive_packet({P, State}) ->
    {FromJID, ToJID} = get_from_to(P),
    Host = ToJID#jid.lserver,
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! {addlog, {recv, FromJID, ToJID, P}},
    {P, State}.

get_from_to(Packet) ->
    case Packet of
	#iq{from = F, to = T} -> {F, T};
	#message{from = F, to = T} -> {F, T};
	#presence{from = F, to = T} -> {F, T}
    end.

add_log(Io, ShowIP, {Orientation, From, To, Packet}, _OSD) ->
    %%{Orientation, Stanza, Direction} = OSD, 
    LocalJID = case Orientation of
		   send -> From;
		   recv -> To
	       end,
    LocalIPS = case ShowIP of
		   true ->
		       case ejabberd_sm:get_user_ip(
			      LocalJID#jid.user,
			      LocalJID#jid.server,
			      LocalJID#jid.resource) of
			   {UserIP, _Port} ->
			       io_lib:format("lip=\"~s\" ", [inet_parse:ntoa(UserIP)]);
			   undefined -> "lip=\"undefined\" "
		       end;
		   false -> ""
	       end,
    TimestampISO = get_now_iso(),
    io:fwrite(Io, "<packet or=\"~p\" ljid=\"~s\" ~sts=\"~s\">~s</packet>~n",
	      [Orientation, binary_to_list(jid:encode(LocalJID)), LocalIPS,
	       binary_to_list(TimestampISO), binary_to_list(fxml:element_to_binary(xmpp:encode(Packet)))]).

%% -------------------
%% File
%% -------------------

open_file(Logdir, Host) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    Logname = str:format("~s-~4..0B~2..0B~2..0BT~2..0B:~2..0B:~2..0B.xml",
          [Host, Year, Month, Day, Hour, Min, Sec]),
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

rotate_log(IoDevice, Logdir, Host) ->
    close_file(IoDevice),
    open_file(Logdir, Host).

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

get_now_iso() ->
    xmpp_util:encode_timestamp(erlang:timestamp()).

calc_div(A, B) when is_integer(A) and is_integer(B) and (B /= 0) ->
    A/B;
calc_div(_A, _B) ->
    0.5. %% This ensures that no rotation is performed

mod_opt_type(stanza) ->
    econf:list(econf:enum([iq, message, presence, other]));
mod_opt_type(direction) ->
    econf:list(econf:enum([internal, vhosts, external]));
mod_opt_type(orientation) ->
    econf:list(econf:enum([send, recv]));
mod_opt_type(logdir) ->
    econf:directory();
mod_opt_type(show_ip) ->
    econf:bool();
mod_opt_type(rotate_days) ->
    econf:non_neg_int();
mod_opt_type(rotate_megs) ->
    econf:non_neg_int();
mod_opt_type(rotate_kpackets) ->
    econf:non_neg_int();
mod_opt_type(check_rotate_kpackets) ->
    econf:non_neg_int().

mod_options(_Host) ->
    [{stanza, [iq, message, presence, other]},
     {direction, [internal, vhosts, external]},
     {orientation, [send, recv]},
     {logdir, "/tmp/jabberlogs/"},
     {show_ip, false},
     {rotate_days, 1},
     {rotate_megs, 10},
     {rotate_kpackets, 10},
     {check_rotate_kpackets, 1}].
