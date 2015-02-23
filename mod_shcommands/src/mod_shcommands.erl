%%%----------------------------------------------------------------------
%%% File    : mod_shcommands.erl
%%% Author  : Badlop <badlop@ono.com>
%%% Purpose : Execute shell commands
%%% Created : 1 Sep 2007 by Badlop <badlop@ono.com>
%%% Id      : $Id: mod_shcommands.erl 1034 2009-11-17 21:44:17Z badlop $
%%%----------------------------------------------------------------------

-module(mod_shcommands).
-author('badlop@ono.com').

-behaviour(gen_mod).

-export([web_menu_node/3, web_page_node/5,
	 start/2, stop/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").

%%-------------------
%% gen_mod functions
%%-------------------

start(_Host, _Opts) ->
    ejabberd_hooks:add(webadmin_menu_node, ?MODULE, web_menu_node, 50),
    ejabberd_hooks:add(webadmin_page_node, ?MODULE, web_page_node, 50),
    ok.

stop(_Host) ->
    ejabberd_hooks:delete(webadmin_menu_node, ?MODULE, web_menu_node, 50),
    ejabberd_hooks:delete(webadmin_page_node, ?MODULE, web_page_node, 50),
    ok.

%%-------------------
%% Web Admin Menu
%%-------------------

web_menu_node(Acc, _Node, Lang) ->
    Acc ++ [{<<"shcommands">>, ?T(<<"Shell Commands">>)}].

%%-------------------
%% Web Admin Page
%%-------------------

web_page_node(_, Node, [<<"shcommands">>], Query, Lang) ->
    Res = [?XC(<<"h1">>, <<"Shell Commands">>) | get_content(Node, Query, Lang)],
    {stop, Res};
web_page_node(Acc, _, _, _, _) -> Acc.

%%-------------------
%% Generate content
%%-------------------

get_content(Node, Query, Lang) ->
    Instruct = ?T("Type a command in a textbox and click Execute."),
    {{CommandCtl, CommandErl, CommandShell}, Res} = case catch parse_and_execute(Query, Node) of
							{'EXIT', _} -> {{"", "", ""}, Instruct};
							Result_tuple -> Result_tuple
						    end,
    TitleHTML = [
		 ?XC(<<"p">>, ?T(<<"Type a command in a textbox and click Execute.  Use only commands which immediately return a result.">>)),
		 ?XC(<<"p">>, ?T(<<"WARNING: Use this only if you know what you are doing.">>))
		],
    CommandHTML =
	[?XAE(<<"form">>, [{<<"method">>, <<"post">>}],
	      [?XAE(<<"table">>, [],
		    [?XE(<<"tbody">>,
			 [?XE(<<"tr">>,
			      [?X(<<"td">>),
			       ?XCT(<<"td">>, <<"ejabberd_ctl">>),
			       ?XE(<<"td">>, [?INPUTS(<<"text">>, <<"commandctl">>, list_to_binary(CommandCtl), <<"70">>),
					  ?INPUTT(<<"submit">>, <<"executectl">>, <<"Execute">>)])
			      ]
			     ),
			  ?XE(<<"tr">>,
			      [?X(<<"td">>),
			       ?XCT(<<"td">>, <<"erlang shell">>),
			       ?XE(<<"td">>, [?INPUTS(<<"text">>, <<"commanderl">>, list_to_binary(CommandErl), <<"70">>),
					  ?INPUTT(<<"submit">>, <<"executeerl">>, <<"Execute">>)])
			      ]
			     ),
			  ?XE(<<"tr">>,
			      [?X(<<"td">>),
			       ?XCT(<<"td">>, <<"system shell">>),
			       ?XE(<<"td">>, [?INPUTS(<<"text">>, <<"commandshe">>, list_to_binary(CommandShell), <<"70">>),
					  ?INPUTT(<<"submit">>, <<"executeshe">>, <<"Execute">>)])
			      ]
			     )
			 ]
			)])]
	     )],
    ResHTML =
	[?XAC(<<"textarea">>, [{<<"wrap">>, <<"off">>}, {<<"style">>, <<"font-family:monospace;">>},
			   {<<"name">>, <<"result">>}, {<<"rows">>, <<"30">>}, {<<"cols">>, <<"80">>}],
	      list_to_binary(Res))
	],
    TitleHTML ++ CommandHTML ++ ResHTML.

parse_and_execute(Query, Node) ->
    {[Exec], _} = lists:partition(
		    fun(ExType) ->
			    lists:keymember(ExType, 1, Query)
		    end,
		    [<<"executectl">>, <<"executeerl">>, <<"executeshe">>]),
    Commands = {get_val(<<"commandctl">>, Query),
		get_val(<<"commanderl">>, Query),
		get_val(<<"commandshe">>, Query)},
    R = parse1_command(Exec, Commands, Node),
    {Commands, R}.

get_val(Val, Query) ->
    {value, {_, R}} = lists:keysearch(Val, 1, Query),
    binary_to_list(R).

parse1_command(<<"executectl">>, {Command, _, _}, Node) ->
    Command2 = string:tokens(Command, " "),
    {_E, Efile} = execute(ctl, Node, Command2),
    io_lib:format("ejabberdctl ~p ~s~n~s", [Node, Command, Efile]);

parse1_command(<<"executeerl">>, {_, Command, _}, Node) ->
    {ok, A2, _} = erl_scan:string(Command),
    {ok, A3} = erl_parse:parse_exprs(A2),
    {E, Efile} = execute(erl, Node, A3),
    io_lib:format("(~p)1> ~s~s~n~p", [Node, Command, Efile, E]);

parse1_command(<<"executeshe">>, {_, _, Command}, Node) ->
    E = rpc:call(Node, os, cmd, [Command]),
    C1 = lists:map(
	   fun(C) -> string:strip(os:cmd(C), right, $\n) end,
	   ["whoami", "hostname -s", "pwd"]),
    io_lib:format("~s@~s:~s$ ~s~n~s", C1 ++ [Command, E]).


execute(Type, Node, C) ->
    GL = group_leader(),
    Filename = "temp" ++ io_lib:format("~p", [random:uniform()*10000]),
    {ok, File} = file:open(Filename, [write]),
    group_leader(File, self()),
    Res = case Type of
	      ctl -> rpc:call(Node, ejabberd_ctl, process, [C]);
	      erl -> rpc:call(Node, erl_eval, exprs, [C, []])
	  end,
    E = case Res of
	    {value, V, _} -> V;
	    O -> O
	end,
    group_leader(GL, self()),
    file:close(File),
    {ok, B} = file:read_file(Filename),
    file:delete(Filename),
    E2 = case binary_to_list(B) of
	     [] -> [];
	     List -> "\n"++List
	 end,
    {E, E2}.
