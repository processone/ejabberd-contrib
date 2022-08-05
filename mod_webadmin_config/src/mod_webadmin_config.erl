%%%----------------------------------------------------------------------
%%% File    : mod_webadmin_config.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Show Configuration in Web Admin
%%% Created : 5 Aug 2022 by Badlop <badlop@process-one.net>
%%%----------------------------------------------------------------------

-module(mod_webadmin_config).
-author('badlop@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0, mod_status/0]).
-export([get_content/3]).
-export([web_menu_node/3, web_page_node/5]).

-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("logger.hrl").
-include("translate.hrl").

-define(OLDFILE, "old.yml").
-define(NEWFILE, "new.yml").

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

depends(_Host, _Opts) ->
    [].

mod_options(_Host) ->
    [].

mod_doc() -> #{}.

mod_status() ->
    "Page available in WebAdmin -> Nodes -> your node -> Configuration".

%%-------------------
%% Web Admin Menu
%%-------------------

web_menu_node(Acc, _Node, Lang) ->
    Acc ++ [{<<"config">>, translate:translate(Lang, ?T("Configuration"))}].

%%-------------------
%% Web Admin Page
%%-------------------

web_page_node(_, Node, [<<"config">>], Query, Lang) ->
    Res = rpc:call(Node, mod_webadmin_config, get_content, [Node, Query, Lang]),
    {stop, Res};
web_page_node(Acc, _, _, _, _) ->
    Acc.

%%-------------------
%% Config Changes
%%-------------------

config_tmp_filename(Filename) ->
    filename:join(filename:dirname(ejabberd_logger:get_log_path()),
                  Filename).

dump_config(Filename) ->
    ejabberd_config:dump(config_tmp_filename(Filename)).

get_last_config_changes() ->
    OldFn = config_tmp_filename(?OLDFILE),
    NewFn = config_tmp_filename(?NEWFILE),
    case {file:read_file(OldFn), file:read_file(NewFn)} of
        {{ok, _}, {ok, _}} ->
            os:cmd("diff -u " ++
                       config_tmp_filename(?OLDFILE) ++
                       " " ++
                       config_tmp_filename(?NEWFILE));
        _ ->
            "Old and/or new configuration not found."
    end.

get_lines_length(Data) ->
    [LongString] = io_lib:format("~s", [lists:flatten(Data)]),
    %% The newline character \n is integer 10 in erlang
    lists:foldl(fun (10, Sum) -> Sum + 1;
                    (_, Sum) -> Sum
                end,
                0,
                LongString
               ).

get_config_paths_els() ->
    Bins = [ejabberd_config:path(),
            ext_mod:config_dir()
           | ext_mod:modules_configs()],
    [?LI([?C(Bin)]) || Bin <- Bins].

%%-------------------
%% Generate content
%%-------------------

get_content(Node, Query, Lang) ->
    Res = node_reload_parse_query(Node, Query, Lang),
    Y = ejabberd_config:get_option(yaml_config),
    Data = fast_yaml:encode(Y),
    LastConfigChanges = get_last_config_changes(),
    ResEl = case Res of
                ok -> [?XREST(?T("Submitted"))];
                {error, ErrorText} -> [?XREST(<<"Error: ", ErrorText/binary>>)];
                nothing -> []
            end,
    ConfigurationPathsEls = get_config_paths_els(),
    ?H1GL(str:translate_and_format(Lang, ?T("Configuration at ~p"), [Node]),
          <<"file-format/#reload-at-runtime">>,
          <<"Reload at Runtime">>) ++
        ResEl ++
        [?XCT(<<"h3">>, ?T("Configuration paths:")),
         ?XE(<<"ul">>, ConfigurationPathsEls),
         ?BR,
         ?XAE(<<"form">>,
              [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
              [?INPUTT(<<"submit">>, <<"reload">>, ?T("Reload Configuration"))]
             ),
         ?XCT(<<"h3">>, ?T("Configuration changes in last reload:")),
         ?XAC(<<"textarea">>,
              [{<<"wrap">>, <<"off">>},
               {<<"style">>, <<"font-family:monospace;">>},
               {<<"class">>, <<"result">>},
               {<<"name">>, <<"result">>},
               {<<"rows">>, integer_to_binary(get_lines_length(LastConfigChanges))},
               {<<"cols">>, <<"80">>}],
              list_to_binary(LastConfigChanges)
             ),
         ?XCT(<<"h3">>, ?T("Configuration in memory:")),
         ?XAC(<<"textarea">>,
              [{<<"wrap">>, <<"off">>},
               {<<"style">>, <<"font-family:monospace;">>},
               {<<"class">>, <<"result">>},
               {<<"name">>, <<"result">>},
               {<<"rows">>, integer_to_binary(get_lines_length(Data))},
               {<<"cols">>, <<"80">>}],
              list_to_binary(Data)
             )
        ].

node_reload_parse_query(Node, Query, _Lang) ->
    case lists:keysearch(<<"reload">>, 1, Query) of
        {value, _} ->
            dump_config(?OLDFILE),
            case ejabberd_cluster:call(Node, ejabberd_config, reload, []) of
                ok ->
                    dump_config(?NEWFILE),
                    ok;
                {error, Error} ->
                    ?ERROR_MSG("~p~n", [Error]),
                    {error, (str:format("~p", [Error]))};
                {badrpc, Error} ->
                    ?ERROR_MSG("Bad RPC: ~p~n", [Error]),
                    {error, <<"Bad RPC: ", (str:format("~p", [Error]))/binary>>}
            end;
        _ ->
            nothing
    end.
