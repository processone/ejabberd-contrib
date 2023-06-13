-module(ejabberd_observer_cli).

-export([start/0, stop/1, mod_status/0]).

start() ->
    application:set_env(observer_cli, plugins, plugins(), [{persistent, true}]),
    observer_cli:start_plugin().

stop(_Host) ->
    ok.

mod_status() ->
    "In an erlang shell run: ejabberd_observer_cli:start().".

plugins() ->
    [
        #{
            module => ejabberd_observer_cli_vhosts,
            title => "VHosts",
            interval => 1600,
            shortcut => "V",
            sort_column => 2
        },
        #{
            module => ejabberd_observer_cli_users,
            title => "Users",
            interval => 1600,
            shortcut => "U",
            sort_column => 2
        },
        %% #{module => ejabberd_observer_cli_userstophost, title => "Users Top Vhost",
        %%   interval => 1600, shortcut => "T", sort_column => 2},
        #{
            module => ejabberd_observer_cli_muc,
            title => "MUC",
            interval => 1600,
            shortcut => "M",
            sort_column => 2
        },
        #{
            module => os_stats_plug,
            title => "OS",
            interval => 2000,
            shortcut => "O",
            sort_column => 2
        }
    ].
