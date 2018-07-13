%%%----------------------------------------------------------------------
%%% File    : mod_stats2file.erl
%%% Author  : Badlop <badlop@ono.com>
%%% Purpose : Generates files with all kind of statistics
%%% Created :
%%% Id      : $Id: mod_stats2file.erl 440 2007-12-06 22:36:21Z badlop $
%%%----------------------------------------------------------------------

-module(mod_stats2file).
-author('badlop@ono.com').

-behaviour(gen_mod).

-export([start/2, loop/5, stop/1]).

-include("xmpp.hrl").
-include("mod_roster.hrl").

-define(PROCNAME, ejabberd_mod_stats2file).
-define(T(Text), translate:translate("Lang", Text)).

%% -------------------
%% Module control
%% -------------------

start(_Host, Opts) ->
	case whereis(?PROCNAME) of
		undefined ->
			Interval = gen_mod:get_opt(interval, Opts, fun(O) -> O end, 5),
			I = Interval*60*1000,
			%I = 9000, %+++

			Type = gen_mod:get_opt(type, Opts, fun(O) -> O end, html),

			Split = gen_mod:get_opt(split, Opts, fun(O) -> O end, false),

			BaseFilename = binary_to_list(gen_mod:get_opt(basefilename, Opts, fun(O) -> O end, "/tmp/ejasta")),

			Hosts_all = ejabberd_config:get_global_option(hosts, fun(O) -> O end),
			Hosts1 = gen_mod:get_opt(hosts, Opts, fun(O) -> O end, Hosts_all),
			Hosts = [binary_to_list(H) ||  H <- Hosts1],

			register(?PROCNAME, spawn(?MODULE, loop, [I, Hosts, BaseFilename, Type, Split]));
		_ ->
			ok
	end,
	ok.

loop(I, Hs, O, T, Split) ->
	write_statsfiles(Split, I, Hs, O, T),
	timer:send_after(I, stats),
	receive
		stats -> ?MODULE:loop(I, Hs, O, T, Split);
		stop -> ok
	end.

stop(_Host) ->
	case whereis(?PROCNAME) of
		undefined -> ok;
		_ -> 
			?PROCNAME ! stop
	end.


%% -------------------
%% write_stat*
%% -------------------

write_statsfiles(false, I, Hs, O, T) ->
	Fn = filename:flatten([O, ".", T]),
	{ok, F} = file:open(Fn, [write]),
	fwini(F, T),
	write_stats(I, server, "", F, T),
	fwbr(F, T),
	fwbr(F, T),
	Node = node(),
	write_stats(I, node, Node, F, T),
	lists:foreach(
		fun(H) -> 
			fwbr(F, T),
			fwbr(F, T),
			write_stats(I, vhost, H, F, T) 
		end, 
		Hs),
	fwend(F, T),
	file:close(F);

write_statsfiles(true, I, Hs, O, T) ->
	write_statsfile(I, server, "", O, T),
	Node = node(),
	write_statsfile(I, node, Node, O, T),
	lists:foreach(
		fun(H) -> 
			write_statsfile(I, vhost, H, O, T) 
		end, 
		Hs).

write_statsfile(I, Class, Name, O, T) ->
	Fn = filename:flatten([O, "-", Class, "-", Name, ".", T]),
	{ok, F} = file:open(Fn, [write]),
	fwini(F, T),
	write_stats(I, Class, Name, F, T),
	fwend(F, T),
	file:close(F).

write_stats(I, server, _Name, F, T) ->
	fwh(F, "Server statistics", 1, T),
	fwbl1(F, T),

	fwtstl(F, "localtime", T),

	fwh(F, "Accounts", 2, T),
	fwbl1(F, T),
	fwttl(F, "registeredusers", T),
	fwbl2(F, T),

	fwh(F, "Roster", 2, T),
	fwbl1(F, T),
	fwttl(F, "totalrosteritems", T),
	fwttl(F, "meanitemsinroster", T),
	fwbl2(F, T),

	fwh(F, "Users", 2, T),
	fwbl1(F, T),
	fwttl(F, "onlineusers", T),
	fwttl(F, "offlinemsg", T),
	fwttl(F, "vcards", T),
	fwbl2(F, T),

	fwh(F, "MUC", 2, T),
	fwbl1(F, T),
	fwttl(F, "totalmucrooms", T),
	fwttl(F, "permmucrooms", T),
	fwttl(F, "regmucrooms", T),
	fwbl2(F, T),

	fwh(F, "Pub/Sub", 2, T),
	fwbl1(F, T),
	fwttl(F, "regpubsubnodes", T),
	fwbl2(F, T),

	%fwh(F, "IRC", 2, T),
	%fwbl1(F, T),
	%fwttl(F, "ircconns", T),
	%fwbl2(F, T),

	fwh(F, "Ratios", 2, T),
	fwbl1(F, T),
	fwttl(F, {"user_login", I}, server, T),
	fwttl(F, {"user_logout", I}, server, T),
	fwttl(F, {"remove_user", I}, server, T),
	fwbl2(F, T),

	fwh(F, "Sessions", 2, T),
	fwbl1(F, T),
	fwh(F, "Clients", 3, T),
	fwbl1(F, T),
	do_stat_table(F, "client", server, T),

	fwbl2(F, T),
	fwh(F, "OS", 3, T),
	fwbl1(F, T),
	do_stat_table(F, "os", server, T),
	fwbl2(F, T),

	case T of
		html -> 
			fwh(F, "Client/OS", 3, T),
			fwbl1(F, T),
			do_stat_table(F, "client_os", server, T),
			fwbl2(F, T),
			
			fwh(F, "Languages", 3, T),
			fwbl1(F, T),
			do_stat_table(F, "languages", server, T),
			fwbl2(F, T),
			fwbl2(F, T);
		_ ->
			ok
	end,

	fwbl2(F, T);

write_stats(I, node, Node, F, T) ->
	fwh(F, "Node statistics", 1, T),
	fwbl1(F, T),

	fwt(F, "Node", Node, T),

	fwh(F, "Connections", 2, T),
	fwbl1(F, T),
	%fwttl(F, "plainusers", T),
	%fwttl(F, "sslusers", T),
	%fwttl(F, "tlsusers", T),
	fwttl(F, "httppollusers", T),
	fwttl(F, "httpbindusers", T),
	fwttl(F, "s2sconnections", T),
	fwttl(F, "s2sservers", T),
	fwbl2(F, T),

	fwh(F, "Erlang", 2, T),
	fwbl1(F, T),
	fwttl(F, "operatingsystem", T),
	fwttl(F, "erlangmachine", T),
	fwttl(F, "erlangmachinetarget", T),
	fwttl(F, "maxprocallowed", T),
	fwttl(F, "totalerlproc", T),
	fwttl(F, "procqueue", T),
	fwbl2(F, T),

	fwh(F, "Times", 2, T),
	fwbl1(F, T),
	%fwttl(F, "uptimehuman", T),
	%fwttl(F, "lastrestart", T),
	
	fwbr(F, T),
	CPUTime = element(1, rpc:call(Node, erlang, statistics, [runtime])),
	fw(F, "~s (ms): ~w",["CPUtime", CPUTime]),
	
	fwbr(F, T),
	MT = trunc(erlang:memory(total)/1024),
	fwt(F, "Memory VM (KB)", MT, T),
	
	fwbl2(F, T),

	fwh(F, "CPU", 2, T),
	fwbl1(F, T),
	fwttl(F, "cpu_avg1", T),
	fwttl(F, "cpu_avg5", T),
	fwttl(F, "cpu_avg15", T),
	fwttl(F, "cpu_nprocs", T),
	U = cpu_sup:util([detailed]),
	fwttl(F, {"cpu_util_user", U}, T),
	fwttl(F, {"cpu_util_nice_user", U}, T),
	fwttl(F, {"cpu_util_kernel", U}, T),
	fwttl(F, {"cpu_util_idle", U}, T),
	fwttl(F, {"cpu_util_wait", U}, T),
	fwbl2(F, T),

	fwh(F, "RAM", 2, T),
	fwbl1(F, T),
	M = memsup:get_system_memory_data(),
	fwttl(F, {"memsup_system", M}, T),
	fwttl(F, {"memsup_free", M}, T),
	fwttl(F, {"reductions", I}, T),
	fwbl2(F, T),

	fwbl2(F, T);

write_stats(I, vhost, Host, F, T) -> 
	fwh(F, "Host statistics", 1, T),
	fwbl1(F, T),

	fwtstl(F, "vhost", Host, T),

	fwh(F, "Accounts", 2, T),
	fwbl1(F, T),
	fwttl(F, "registeredusers", Host, T),
	fwbl2(F, T),

	fwh(F, "Roster", 2, T),
	fwbl1(F, T),
	fwttl(F, "totalrosteritems", Host, T),
	fwttl(F, "meanitemsinroster", Host, T),
	fwbl2(F, T),

	fwh(F, "Users", 2, T),
	fwbl1(F, T),
	fwttl(F, "onlineusers", Host, T),
	fwttl(F, "offlinemsg", Host, T),
	fwttl(F, "vcards", Host, T),
	fwbl2(F, T),

	fwh(F, "Connections", 2, T),
	fwbl1(F, T),
	fwttl(F, "s2sconnections", Host, T),
	fwbl2(F, T),

	fwh(F, "MUC", 2, T),
	fwbl1(F, T),
	fwttl(F, "totalmucrooms", Host, T),
	fwttl(F, "permmucrooms", Host, T),
	fwttl(F, "regmucrooms", Host, T),
	fwbl2(F, T),

	%fwh(F, "IRC", 2, T),
	%fwbl1(F, T),
	%fwttl(F, "ircconns", Host, T),
	%fwbl2(F, T),

	fwh(F, "Sessions", 2, T),
	fwbl1(F, T),
	fwh(F, "Clients", 3, T),
	fwbl1(F, T),
	do_stat_table(F, "client", Host, T),

	fwbl2(F, T),
	fwh(F, "OS", 3, T),
	fwbl1(F, T),
	do_stat_table(F, "os", Host, T),
	fwbl2(F, T),

	case T of
		html -> 
			fwh(F, "Client/OS", 3, T),
			fwbl1(F, T),
			do_stat_table(F, "client_os", Host, T),
			fwbl2(F, T),
			
			fwh(F, "Languages", 3, T),
			fwbl1(F, T),
			do_stat_table(F, "languages", Host, T),
			fwbl2(F, T),
			fwbl2(F, T);
		_ ->
			ok
	end,

	fwh(F, "Ratios", 2, T),
	fwbl1(F, T),
	fwttl(F, {"user_login", I}, Host, T),
	fwttl(F, {"user_logout", I}, Host, T),
	fwttl(F, {"remove_user", I}, Host, T),
	fwttl(F, {send, iq, in, I}, Host, T),
	fwttl(F, {send, iq, out, I}, Host, T),
	fwttl(F, {send, message, in, I}, Host, T),
	fwttl(F, {send, message, out, I}, Host, T),
	fwttl(F, {send, presence, in, I}, Host, T),
	fwttl(F, {send, presence, out, I}, Host, T),
	fwttl(F, {recv, iq, in, I}, Host, T),
	fwttl(F, {recv, iq, out, I}, Host, T),
	fwttl(F, {recv, message, in, I}, Host, T),
	fwttl(F, {recv, message, out, I}, Host, T),
	fwttl(F, {recv, presence, in, I}, Host, T),
	fwttl(F, {recv, presence, out, I}, Host, T),
	fwbl2(F, T),

	fwbl2(F, T).

%% -------------------
%% get(*
%% -------------------

fwttl(F, Arg, T) -> fwt(F, gett(Arg), getl(Arg), T).
fwttl(F, Arg, Host, T) -> fwt(F, gett(Arg), getl(Arg, Host), T).

fwtstl(F, Arg, T) -> fwts(F, gett(Arg), getl(Arg), T).
fwtstl(F, Arg, Host, T) -> fwts(F, gett(Arg), getl(Arg, Host), T).

gett(Arg) -> get(node(), [Arg, title]).
getl(Args) -> get(node(), [Args]).
getl(Args, Host) -> get(node(), [Args, Host]).

get(Node, A) -> 
	mod_statsdx:get_statistic(Node, A).


%% -------------------
%% utilities
%% -------------------

fw(F, S) -> file:write(F, S).
fw(F, S, O) -> file:write(F, io_lib:format(S, O)).

fwt(F, S, O, html) -> fw(F, "~s: ~p<br/>~n",[S, O]);
fwt(F, S, O, txt) -> fw(F, "~s: ~p~n",[S, O]);
fwt(F, _, O, dat) -> fw(F, "~p~n",[O]).

fwts(F, S, O, html) -> fw(F, "~s: ~s<br/>~n",[S, O]);
fwts(F, S, O, txt) -> fw(F, "~s: ~s~n",[S, O]);
fwts(F, _, O, dat) -> fw(F, "~s~n",[O]).

%fwtsn(F, S, O, html) -> fw(F, "~s: ~s<br/>",[?T(S), O]);
%fwtsn(F, S, O, txt) -> fw(F, "~s: ~s",[?T(S), O]);
%fwtsn(F, _, O, dat) -> fw(F, "~s",[O]).

fwh(F, S, N, html) -> fw(F, "<h~p>~s</h~p>~n",[N, S, N]);
fwh(F, S, 1, txt) -> fw(F, "     ===== ~s =====~n",[S]);
fwh(F, S, 2, txt) -> fw(F, "~n   --- ~s ---~n",[S]);
fwh(F, S, 3, txt) -> fw(F, "~n     + ~s +~n",[S]);
fwh(_, _, _, dat) -> ok.

fwbr(F, html) -> fw(F, "<br/>\n");
fwbr(F, txt) -> fw(F, "\n");
fwbr(_, dat) -> ok.

fwini(F, html) -> fw(F, "<body>\n");
fwini(_, txt) -> ok;
fwini(_, dat) -> ok.
fwend(F, html) -> fw(F, "</body>\n");
fwend(_, txt) -> ok;
fwend(_, dat) -> ok.

fwbl1(F, html) -> fw(F, "<blockquote>\n");
fwbl1(_, txt) -> ok;
fwbl1(_, dat) -> ok.
fwbl2(F, html) -> fw(F, "</blockquote>\n");
fwbl2(_, txt) -> ok;
fwbl2(_, dat) -> ok.

do_stat_table(F, Stat, Host, T) ->
	do_stat_table(F, Stat, Host, T, unknown).
do_stat_table(F, Stat, Host, T, _Lang) ->
	lists:map(
		fun({Name, Value}) -> 
			fwts(F, Name, io_lib:format("~p", [Value]), T)
		end,
		mod_statsdx:get_statistic(global, [Stat, Host])
	).
