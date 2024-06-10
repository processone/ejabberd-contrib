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

-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0, mod_status/0]).
-export([execute_system/1, execute_erlang/1]).
-export([web_menu_node/3, web_page_node/3,
         web_page_node/5 % ejabberd 24.02 or older
        ]).

-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_commands.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("translate.hrl").

%%-------------------
%% gen_mod functions
%%-------------------

start(_Host, _Opts) ->
    ejabberd_hooks:add(webadmin_menu_node, ?MODULE, web_menu_node, 50),
    ejabberd_hooks:add(webadmin_page_node, ?MODULE, web_page_node, 50),
    ejabberd_commands:register_commands(get_commands_spec()),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(webadmin_menu_node, ?MODULE, web_menu_node, 50),
    ejabberd_hooks:delete(webadmin_page_node, ?MODULE, web_page_node, 50),
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:unregister_commands(get_commands_spec());
        true ->
            ok
    end.

depends(_Host, _Opts) ->
    [].

mod_options(_Host) ->
    [].

mod_doc() -> #{}.

mod_status() ->
    "Page available in WebAdmin -> Nodes -> your node -> Shell Commands".

%%-------------------
%% Commands
%%-------------------

get_commands_spec() ->
    [#ejabberd_commands{name = execute_system, tags = [system],
			desc = "Execute a system shell command",
			policy = admin,
			module = ?MODULE, function = execute_system,
                        args_desc = ["Command to execute in the system shell"],
                        args_example = ["uptime"],
                        result_example = "12:02:10 up  1:34,  7 users,  load average: 0,58, 0,56, 0,61",
			args = [{command, string}],
			result = {res, string}},
     #ejabberd_commands{name = execute_erlang, tags = [system, erlang],
			desc = "Execute an erlang shell command",
			policy = admin,
			module = ?MODULE, function = execute_erlang,
			args = [{command, string}],
                        args_desc = ["Command to execute in the Erlang shell"],
                        args_example = ["erlang:system_info(system_version)."],
                        result_example = "Erlang/OTP 22 [erts-10.7] [64-bit] [smp:2:2]\n",
			result = {res, string}} ].

execute_system(Command) ->
    {E, _} = parse1_command(<<"executeshe">>, {none, none, Command}, node()),
    lists:flatten(E).

execute_erlang(Command) ->
    {E, _} = parse1_command(<<"executeerl">>, {none, Command, none}, node()),
    lists:flatten(io_lib:format("~p", [E])).

%%-------------------
%% Web Admin Menu
%%-------------------

web_menu_node(Acc, _Node, Lang) ->
    Acc ++ [{<<"shcommands">>, translate:translate(Lang, ?T("Shell Commands"))}].

%%-------------------
%% Web Admin Page
%%-------------------

%% ejabberd 24.02 or older
web_page_node(Acc, Node, Path, Query, Lang) ->
    web_page_node(Acc, Node, #request{method = 'GET',
                                      raw_path = <<"">>,
                                      ip = {{127,0,0,1}, 0},
                                      sockmod = 'gen_tcp',
                                      socket = hd(erlang:ports()),
                                      path = Path, q = Query, lang = Lang}).

web_page_node(_, Node, #request{path = [<<"shcommands">>], q = Query, lang = Lang}) ->
    Res = [?XC(<<"h1">>, translate:translate(Lang, ?T("Shell Commands"))) | get_content(Node, Query, Lang)],
    {stop, Res};
web_page_node(Acc, _, _) -> Acc.

%%-------------------
%% Generate content
%%-------------------

get_content(Node, Query, Lang) ->
    Instruct = translate:translate(Lang, ?T("Type a command in a textbox and click Execute.")),
    {{CommandCtl, CommandErl, CommandShell}, Res} = case catch parse_and_execute(Query, Node) of
							{'EXIT', _} -> {{"", "", ""}, Instruct};
							Result_tuple -> Result_tuple
						    end,
    TitleHTML = [
		 ?XC(<<"p">>, translate:translate(Lang, ?T("Type a command in a textbox and click Execute."))),
		 ?XC(<<"p">>, translate:translate(Lang, ?T("Use only commands which immediately return a result."))),
		 ?XC(<<"p">>, translate:translate(Lang, ?T("WARNING: Use this only if you know what you are doing.")))
		],
    CommandHTML =
	[?XAE(<<"form">>, [{<<"method">>, <<"post">>}],
	      [?XAE(<<"table">>, [],
		    [?XE(<<"tbody">>,
			 [?XE(<<"tr">>,
			      [?X(<<"td">>),
			       ?XCT(<<"td">>, <<"ejabberd_ctl">>),
			       ?XE(<<"td">>, [?INPUTS(<<"text">>, <<"commandctl">>, list_to_binary(CommandCtl), <<"70">>),
					  ?INPUTT(<<"submit">>, <<"executectl">>, translate:translate(Lang, ?T("Execute")))])
			      ]
			     ),
			  ?XE(<<"tr">>,
			      [?X(<<"td">>),
			       ?XCT(<<"td">>, <<"erlang shell">>),
			       ?XE(<<"td">>, [?INPUTS(<<"text">>, <<"commanderl">>, list_to_binary(CommandErl), <<"70">>),
					  ?INPUTT(<<"submit">>, <<"executeerl">>, translate:translate(Lang, ?T("Execute")))])
			      ]
			     ),
			  ?XE(<<"tr">>,
			      [?X(<<"td">>),
			       ?XCT(<<"td">>, <<"system shell">>),
			       ?XE(<<"td">>, [?INPUTS(<<"text">>, <<"commandshe">>, list_to_binary(CommandShell), <<"70">>),
					  ?INPUTT(<<"submit">>, <<"executeshe">>, translate:translate(Lang, ?T("Execute")))])
			      ]
			     )
			 ]
			)])]
	     )],
    ResHTML =
	[?XAC(<<"textarea">>, [{<<"wrap">>, <<"off">>}, {<<"style">>, <<"font-family:monospace;">>},
			   {<<"name">>, <<"result">>}, {<<"rows">>, <<"30">>}, {<<"cols">>, <<"80">>}],
	      Res)
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
    {_, R} = parse1_command(Exec, Commands, Node),
    {Commands, R}.

get_val(Val, Query) ->
    {value, {_, R}} = lists:keysearch(Val, 1, Query),
    binary_to_list(R).

parse1_command(<<"executectl">>, {Command, _, _}, Node) ->
    Command2 = string:tokens(Command, " "),
    {_E, Efile} = execute(ctl, Node, Command2),
    {Efile, io_lib:format("ejabberdctl ~p ~s~n~s", [Node, Command, Efile])};

parse1_command(<<"executeerl">>, {_, Command, _}, Node) ->
    {ok, A2, _} = erl_scan:string(Command),
    {ok, A3} = erl_parse:parse_exprs(A2),
    {E, Efile} = execute(erl, Node, A3),
    {E, io_lib:format("(~p)1> ~s~s~n~p", [Node, Command, Efile, E])};

parse1_command(<<"executeshe">>, {_, _, Command}, Node) ->
    E = rpc:call(Node, os, cmd, [Command]),
    C1 = lists:map(
	   fun(C) -> string:strip(os:cmd(C), right, $\n) end,
	   ["whoami", "hostname -s", "pwd"]),
    {E, io_lib:format("~s@~s:~s$ ~s~n~s", C1 ++ [Command, E])}.


execute(Type, Node, C) ->
    GL = group_leader(),
    Filename = <<"temp", (p1_rand:get_string())/binary>>,
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
