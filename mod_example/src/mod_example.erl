%%%-------------------------------------------------------------------
%%% File    : mod_example.erl
%%% Author  : Author Name <name@example.org>
%%% Purpose : Example ejabberd module
%%% Created :
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
%%%-------------------------------------------------------------------

%%%% definitions

%% @format-begin

-module(mod_example).

-author('name@example.org').

%%
%% gen_mod

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, mod_options/1, depends/2, mod_doc/0]).

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

%%
%% commands

-export([get_commands_spec/0]).
-export([apiversion0/0, apiversion2/0, apizero/0, apione/0, integer/1, string/1, binary/1,
         tuple/1, list/1, list_tuple/1, atom/1, rescode/1, restuple/2, all/6]).

-include("ejabberd_commands.hrl").
-include("translate.hrl").

%%
%% hooks

%% run event
-export([run_hook_global/0, run_hook_host/1, run_hook_fold_global/0,
         run_hook_fold_host/1]).
%% add hook
-export([example_hook_global_function1/2, example_hook_global_function2/2,
         example_hook_global_function3/2]).
-export([example_hook_host_subs/5]).
-export([example_hook_host_function1/2, example_hook_host_function2/2,
         example_hook_host_function3/2]).
-export([example_hook_fold_global_function1/3, example_hook_fold_global_function2/3,
         example_hook_fold_global_function3/3]).
-export([example_hook_fold_host_function1/3, example_hook_fold_host_function2/3,
         example_hook_fold_host_function3/3]).

%%
%% iqdisc

%%
%% webadmin

-export([webadmin_menu_main/2, webadmin_page_main/2, webadmin_menu_host/3,
         webadmin_page_host/3, webadmin_menu_node/3, webadmin_page_node/3,
         webadmin_menu_hostnode/4, webadmin_page_hostnode/4, webadmin_menu_hostuser/4,
         webadmin_page_hostuser/4]).

-import(ejabberd_web_admin, [make_command/4, make_command_raw_value/3, make_table/4]).

-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").

%%%==================================
%%%% gen_mod

start(_Host, _Opts) ->
    %register_iq_handlers(Host),
    {ok,
     [{commands, get_commands_spec()},
      {commands, get_hooks_commands_spec()},
      %% global
      {hook, example_hook_global, example_hook_global_function1, 81, global},
      {hook, example_hook_global, example_hook_global_function2, 82, global},
      {hook, example_hook_global, example_hook_global_function3, 83, global},
      %% host
      {hook, example_hook_host, example_hook_host_function1, 81},
      {hook, example_hook_host, example_hook_host_function2, 82},
      {hook, example_hook_host, example_hook_host_function3, 83},
      {hook_subscribe, example_hook_host, example_hook_host_subs, []},
      %% global fold
      {hook, example_hook_fold_global, example_hook_fold_global_function1, 81, global},
      {hook, example_hook_fold_global, example_hook_fold_global_function2, 82, global},
      {hook, example_hook_fold_global, example_hook_fold_global_function3, 83, global},
      %% host fold
      {hook, example_hook_fold_host, example_hook_fold_host_function1, 81},
      {hook, example_hook_fold_host, example_hook_fold_host_function2, 82},
      {hook, example_hook_fold_host, example_hook_fold_host_function3, 83},
      %% webadmin
      {hook, webadmin_menu_main, webadmin_menu_main, 50, global},
      {hook, webadmin_page_main, webadmin_page_main, 50, global},
      {hook, webadmin_menu_host, webadmin_menu_host, 50},
      {hook, webadmin_page_host, webadmin_page_host, 50},
      {hook, webadmin_menu_node, webadmin_menu_node, 50, global},
      {hook, webadmin_page_node, webadmin_page_node, 50, global},
      {hook, webadmin_menu_hostnode, webadmin_menu_hostnode, 50},
      {hook, webadmin_page_hostnode, webadmin_page_hostnode, 50},
      {hook, webadmin_menu_hostuser, webadmin_menu_hostuser, 50},
      {hook, webadmin_page_hostuser, webadmin_page_hostuser, 50}]}.

stop(_Host) ->
    %unregister_iq_handlers(Host),
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

mod_options(_) ->
    [].

mod_doc() ->
    #{desc =>
          [?T("This is an example module."),
           "",
           ?T("Here you can also find example code for some ejabberd features:"),
           "",
           ?T("- commands"),
           ?T("- hooks"),
           ?T("- iqdisc"),
           ?T("- WebAdmin"),
           "",
           ?T("You can copy this file, remove the unnecessary code, "
              "adapt the useful one and add your features.")],
      example =>
          [{?T("This is an example configuration:"),
            ["modules:", "  mod_example:", "    - resource: \"modadminextraf8x,31ad\""]},
           {?T("Call to srg-create using double-quotes and single-quotes:"),
            ["ejabberdctl srg-create g1 example.org \"\'Group number 1\'\" this_is_g1 g1"]}]}.

%%%==================================
%%%% commands: define

get_commands_spec() ->
    [#ejabberd_commands{name = command_test_apiversion,
                        tags = [test],
                        desc = "Command to test apiversion",
                        module = ?MODULE,
                        function = apiversion0,
                        version = 0,
                        args = [],
                        result = {apiversion, integer},
                        args_example = [],
                        result_example = 0},
     #ejabberd_commands{name = command_test_apiversion,
                        tags = [test],
                        desc = "Command to test apiversion",
                        module = ?MODULE,
                        function = apiversion2,
                        version = 2,
                        args = [],
                        result = {apiversion, integer},
                        args_example = [],
                        result_example = 2},
     #ejabberd_commands{name = command_test_apizero,
                        tags = [test],
                        desc = "Command to test apizero",
                        module = ?MODULE,
                        function = apizero,
                        args = [],
                        result = {apiversion, integer},
                        args_example = [],
                        result_example = 0},
     #ejabberd_commands{name = command_test_apione,
                        tags = [test],
                        desc = "Command to test apione",
                        module = ?MODULE,
                        function = apione,
                        version = 1,
                        args = [],
                        result = {apiversion, integer},
                        args_example = [],
                        result_example = 0},
     #ejabberd_commands{name = command_test_integer,
                        tags = [test],
                        desc = "Command to test integer",
                        module = ?MODULE,
                        function = integer,
                        args = [{arg_integer, integer}],
                        result = {res_integer, integer},
                        args_example = [7],
                        result_example = 7},
     #ejabberd_commands{name = command_test_string,
                        tags = [test],
                        desc = "Command to test string",
                        module = ?MODULE,
                        function = string,
                        args = [{arg_string, string}],
                        result = {res_string, string},
                        args_example = ["Some string."],
                        result_example = "Some string"},
     #ejabberd_commands{name = command_test_binary,
                        tags = [test],
                        desc = "Command to test binary",
                        module = ?MODULE,
                        function = binary,
                        args = [{arg_binary, binary}],
                        result = {res_string, string},
                        args_example = ["Some binary."],
                        result_example = "Some binary"},
     #ejabberd_commands{name = command_test_tuple,
                        tags = [test],
                        desc = "Command to test tuple",
                        module = ?MODULE,
                        function = tuple,
                        args =
                            [{arg_tuple,
                              {tuple,
                               [{element1, string}, {element2, string}, {element3, string}]}}],
                        result =
                            {res_tuple,
                             {tuple, [{element1, string}, {element2, string}, {element3, string}]}},
                        args_example = [{"one", "first", "primary"}],
                        result_example = {"one", "first", "primary"}},
     #ejabberd_commands{name = command_test_list,
                        tags = [test],
                        desc = "Command to test list",
                        module = ?MODULE,
                        function = list,
                        args = [{arg_list, {list, {element, string}}}],
                        result = {res_list, {list, {element, string}}},
                        args_example = [["one", "two", "three"]],
                        result_example = ["one", "two", "three"]},
     #ejabberd_commands{name = command_test_list_tuple,
                        tags = [test],
                        desc = "Command to test list of tuples",
                        module = ?MODULE,
                        function = list_tuple,
                        args =
                            [{arg_list,
                              {list,
                               {list_tuple, {tuple, [{element1, string}, {element2, string}]}}}}],
                        result =
                            {res_list,
                             {list,
                              {list_tuple, {tuple, [{element1, string}, {element2, string}]}}}},
                        args_example = [[{"one", "uno"}, {"two", "dos"}, {"three", "tres"}]],
                        result_example = [{"one", "uno"}, {"two", "dos"}, {"three", "tres"}]},
     #ejabberd_commands{name = command_test_atom,
                        tags = [test],
                        desc = "Command to test atom result",
                        module = ?MODULE,
                        function = atom,
                        args = [{arg_string, string}],
                        result = {res_atom, atom},
                        args_example = ["Some string-then-atom."],
                        result_example = 'Some string-then-atom'},
     #ejabberd_commands{name = command_test_rescode,
                        tags = [test],
                        desc = "Command to test rescode result",
                        module = ?MODULE,
                        function = rescode,
                        args = [{code, string}],
                        result = {res_atom, rescode},
                        args_example = ["ok"],
                        result_example = 0},
     #ejabberd_commands{name = command_test_restuple,
                        tags = [test],
                        desc = "Command to test restuple result",
                        module = ?MODULE,
                        function = restuple,
                        args = [{code, string}, {text, string}],
                        result = {res_atom, restuple},
                        args_example = ["ok", "Some result text"],
                        result_example = {ok, <<"Some result text">>}},
     #ejabberd_commands{name = command_test_all,
                        tags = [test],
                        desc =
                            "Test command that requires all argument types and returns all result types",
                        module = ?MODULE,
                        function = all,
                        args =
                            [{arg_integer, integer},
                             {arg_string, string},
                             {arg_binary, binary},
                             {arg_tuple,
                              {tuple, [{tuple_integer, integer}, {tuple_string, string}]}},
                             {arg_list, {list, {list_string, string}}},
                             {arg_listtuple,
                              {list,
                               {list_tuple,
                                {tuple,
                                 [{list_tuple_integer, integer}, {list_tuple_string, string}]}}}}],
                        result =
                            {response,
                             {tuple,
                              [{arg_integer, integer},
                               {arg_string, string},
                               {arg_atom, atom},
                               {arg_tuple,
                                {tuple, [{tuple_integer, integer}, {tuple_string, string}]}},
                               {arg_list, {list, {list_string, string}}},
                               {arg_listtuple,
                                {list,
                                 {list_tuple,
                                  {tuple,
                                   [{list_tuple_integer, integer},
                                    {list_tuple_string, string}]}}}}]}}}].

%%%==================================
%%%% commands: implement

apiversion0() ->
    0.

apiversion2() ->
    2.

apizero() ->
    0.

apione() ->
    1.

integer(Integer) when is_integer(Integer) ->
    Integer.

string([Char | _] = String) when is_list(String) and is_integer(Char) ->
    String.

binary(Binary) when is_binary(Binary) ->
    Binary.

tuple({T1, T2, T3}) ->
    {T1, T2, T3}.

list([Head | _] = List) when is_list(List) and is_list(Head) ->
    List.

list_tuple(ListTuple) when is_list(ListTuple) ->
    ListTuple.

atom(String) when is_list(String) ->
    list_to_atom(String).

rescode(Code) when Code == "true"; Code == "ok" ->
    list_to_atom(Code);
rescode(Other) ->
    Other.

restuple(Code, Text) when Code == "true"; Code == "ok" ->
    {list_to_atom(Code), Text};
restuple(Other, Text) ->
    {list_to_atom(Other), Text}.

all(Integer,
    [Char | _] = String,
    Binary,
    {_T1, _T2} = Tuple,
    [Head | _] = List,
    ListTuple)
    when is_integer(Integer)
         and is_list(String)
         and is_integer(Char)
         and is_binary(Binary)
         and is_list(List)
         and is_list(Head)
         and is_list(ListTuple) ->
    {Integer, String, misc:binary_to_atom(Binary), Tuple, List, ListTuple}.

%%%==================================
%%%% hooks: run hooks

get_hooks_commands_spec() ->
    [#ejabberd_commands{name = run_hook_global,
                        tags = [test],
                        module = ?MODULE,
                        function = run_hook_global,
                        args = [],
                        result = {res, rescode}},
     #ejabberd_commands{name = run_hook_host,
                        tags = [test],
                        module = ?MODULE,
                        function = run_hook_host,
                        args = [{host, binary}],
                        result = {res, rescode}},
     #ejabberd_commands{name = run_hook_fold_global,
                        tags = [test],
                        module = ?MODULE,
                        function = run_hook_fold_global,
                        args = [],
                        result = {functions, {list, {function, string}}}},
     #ejabberd_commands{name = run_hook_fold_host,
                        tags = [test],
                        module = ?MODULE,
                        function = run_hook_fold_host,
                        args = [{host, binary}],
                        result = {functions, {list, {function, string}}}}].

run_hook_global() ->
    Arg1 = "arg1",
    Arg2 = "arg2",
    ejabberd_hooks:run(example_hook_global, [Arg1, Arg2]).

run_hook_host(Host) ->
    Arg1 = "arg1",
    Arg2 = "arg2",
    ejabberd_hooks:run(example_hook_host, Host, [Arg1, Arg2]).

run_hook_fold_global() ->
    Arg1 = "arg1",
    Arg2 = "arg2",
    ejabberd_hooks:run_fold(example_hook_fold_global, [], [Arg1, Arg2]).

run_hook_fold_host(Host) ->
    Arg1 = "arg1",
    Arg2 = "arg2",
    ejabberd_hooks:run_fold(example_hook_fold_host, Host, [], [Arg1, Arg2]).

%%%==================================
%%%% hooks: add global

example_hook_global_function1(Arg1, Arg2) ->
    io:format("~nexample_hook_global_function1: ~n"
              "  Arg1: ~p~n  Arg2: ~p~n"
              "  continue...~n",
              [Arg1, Arg2]).

example_hook_global_function2(Arg1, Arg2) ->
    io:format("~nexample_hook_global_function2: ~n"
              "  Arg1: ~p~n  Arg2: ~p~n"
              "  stop.~n~n",
              [Arg1, Arg2]),
    stop.

%% As function2 returns stop, function3 will not be called

example_hook_global_function3(Arg1, Arg2) ->
    io:format("~nexample_hook_global_function3: ~n"
              "  Arg1: ~p~n  Arg2: ~p~n~n",
              [Arg1, Arg2]).

%%%==================================
%%%% hooks: add host

example_hook_host_subs(InitArgs, Time, Host, Hook, Args) ->
    io:format("~nexample_hook_host_subs: ~n"
              "  InitArgs: ~p~n"
              "  Time: ~p~n"
              "  Host: ~p~n"
              "  Hook: ~p~n"
              "  Args: ~p~n",
              [InitArgs, Time, Host, Hook, Args]),
    ok.

example_hook_host_function1(Arg1, Arg2) ->
    io:format("~nexample_hook_host_function1: ~n"
              "  Arg1: ~p~n  Arg2: ~p~n"
              "  continue...~n",
              [Arg1, Arg2]).

example_hook_host_function2(Arg1, Arg2) ->
    io:format("~nexample_hook_host_function2: ~n"
              "  Arg1: ~p~n  Arg2: ~p~n"
              "  stop.~n~n",
              [Arg1, Arg2]),
    stop.

%% As function2 returns stop, function3 will not be called

example_hook_host_function3(Arg1, Arg2) ->
    io:format("~nexample_hook_host_function3: ~n"
              "  Arg1: ~p~n  Arg2: ~p~n~n",
              [Arg1, Arg2]).

%%%==================================
%%%% hooks: add global fold

example_hook_fold_global_function1(Acc0, Arg1, Arg2) ->
    io:format("~nexample_hook_fold_global_function1: ~n"
              "  Acc: ~p~n  Arg1: ~p~n  Arg2: ~p~n"
              "  continue...~n",
              [Acc0, Arg1, Arg2]),
    [example_hook_fold_global_function1 | Acc0].

example_hook_fold_global_function2(Acc0, Arg1, Arg2) ->
    io:format("~nexample_hook_fold_global_function2: ~n"
              "  Acc: ~p~n  Arg1: ~p~n  Arg2: ~p~n"
              "  stop.~n~n",
              [Acc0, Arg1, Arg2]),
    {stop, [example_hook_fold_global_function2 | Acc0]}.

%% As function2 returns {stop, _}, function3 will not be called

example_hook_fold_global_function3(Acc0, Arg1, Arg2) ->
    io:format("~nexample_hook_fold_global_function3: ~n"
              "  Acc: ~p~n  Arg1: ~p~n  Arg2: ~p~n~n",
              [Acc0, Arg1, Arg2]),
    [example_hook_fold_global_function3 | Acc0].

%%%==================================
%%%% hooks: add host fold

example_hook_fold_host_function1(Acc0, Arg1, Arg2) ->
    io:format("~nexample_hook_fold_host_function1: ~n"
              "  Acc: ~p~n  Arg1: ~p~n  Arg2: ~p~n"
              "  continue...~n",
              [Acc0, Arg1, Arg2]),
    [example_hook_fold_host_function1 | Acc0].

example_hook_fold_host_function2(Acc0, Arg1, Arg2) ->
    io:format("~nexample_hook_fold_host_function2: ~n"
              "  Acc: ~p~n  Arg1: ~p~n  Arg2: ~p~n"
              "  stop.~n~n",
              [Acc0, Arg1, Arg2]),
    {stop, [example_hook_fold_host_function2 | Acc0]}.

%% As function2 returns {stop, _}, function3 will not be called

example_hook_fold_host_function3(Acc0, Arg1, Arg2) ->
    io:format("~nexample_hook_fold_host_function3: ~n"
              "  Acc: ~p~n  Arg1: ~p~n  Arg2: ~p~n~n",
              [Acc0, Arg1, Arg2]),
    [example_hook_fold_host_function3 | Acc0].

%%%==================================
%%%% iqdisc

%register_iq_handlers(Host) ->
%    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PING, ?MODULE, iq_ping),
%    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PING, ?MODULE, iq_ping).

%unregister_iq_handlers(Host) ->
%    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PING),
%    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PING).

%%%==================================
%%%% webadmin

%%---------------
%% WebAdmin Main

webadmin_menu_main(Acc, Lang) ->
    Acc ++ [{<<"example">>, translate:translate(Lang, ?T("Example Main"))}].

webadmin_page_main(_, #request{path = [<<"example">>]} = R) ->
    Title = [?XC(<<"h1">>, <<"Example Main Page">>)],
    Get = [?X(<<"hr">>),
           make_command(command_test_apiversion, R, [], []),
           make_command(command_test_apizero, R, [], []),
           make_command(command_test_apione, R, [], []),
           ?X(<<"hr">>),
           make_command(command_test_atom, R, [], []),
           make_command(command_test_integer, R, [], []),
           make_command(command_test_string, R, [], []),
           make_command(command_test_binary, R, [], []),
           ?X(<<"hr">>),
           make_command(command_test_rescode, R, [], []),
           make_command(command_test_restuple, R, [], []),
           ?X(<<"hr">>),
           make_command(command_test_tuple, R, [], []),
           make_command(command_test_list, R, [], []),
           make_command(command_test_list_tuple, R, [], []),
           make_command(command_test_all, R, [], [])],
    {stop, Title ++ Get};
webadmin_page_main(Acc, _) ->
    Acc.

%%---------------
%% WebAdmin Host

webadmin_menu_host(Acc, _Host, Lang) ->
    Acc ++ [{<<"example">>, translate:translate(Lang, ?T("Example Host"))}].

webadmin_page_host(_, Host, #request{path = [<<"example">> | _RPath]}) ->
    Title = [?XC(<<"h1">>, <<"Example Host Page: ", Host/binary>>)],
    {stop, Title};
webadmin_page_host(Acc, _, _) ->
    Acc.

%%--------------------
%% WebAdmin Node

webadmin_menu_node(Acc, _Node, _Lang) ->
    Acc ++ [{<<"example">>, <<"Example Node">>}].

webadmin_page_node(_, Node, #request{path = [<<"example">>]}) ->
    NodeBin = atom_to_binary(Node),
    Title = [?XC(<<"h1">>, <<"Example Node Page: ", NodeBin/binary>>)],
    {stop, Title};
webadmin_page_node(Acc, _, _) ->
    Acc.

%%--------------------
%% WebAdmin Host User

webadmin_menu_hostuser(Acc, _Host, _Username, _Lang) ->
    Acc ++ [{<<"example-user">>, <<"Example User">>}].

webadmin_page_hostuser(_, Host, User, #request{path = [<<"example-user">>]}) ->
    Title = [?XC(<<"h1">>, <<"Example Host-User Page: ", Host/binary, " - ", User/binary>>)],
    {stop, Title};
webadmin_page_hostuser(Acc, _, _, _) ->
    Acc.

%%--------------------
%% WebAdmin Host Node

webadmin_menu_hostnode(Acc, _Host, _Username, _Lang) ->
    Acc ++ [{<<"example-host-node">>, <<"Example Host Node">>}].

webadmin_page_hostnode(_, Host, Node, #request{path = [<<"example-host-node">>]}) ->
    NodeBin = atom_to_binary(Node),
    Title =
        [?XC(<<"h1">>, <<"Example Host-Node Page: ", Host/binary, " - ", NodeBin/binary>>)],
    {stop, Title};
webadmin_page_hostnode(Acc, _Host, _Node, _Request) ->
    Acc.

%%%==================================

%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
