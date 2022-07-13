-module(ejabberd_observer_cli_vhosts).

%% observer_cli_plugin Callback API
-export([attributes/1, sheet_header/0, sheet_body/1]).

attributes(PrevState) ->
    Hosts = length(ejabberd_option:hosts()),
    RegisteredUsers =
        lists:foldl(
            fun(Host, Sum) ->
                ejabberd_auth:count_users(Host) + Sum
            end,
            0,
            ejabberd_option:hosts()
        ),
    Sessions = length(ejabberd_sm:dirty_get_sessions_list()),
    SessionsThisNode = length(ejabberd_sm:dirty_get_my_sessions_list()),

    OnlineRoomsNumber = lists:foldl(
        fun(Host, Acc) ->
            Acc + mod_muc:count_online_rooms(Host)
        end,
        0,
        mod_muc_admin:find_hosts(global)
    ),

    Attrs = [
        [
            #{content => "Virtual Hosts", width => 25},
            #{content => Hosts, width => 8},
            #{content => "Sessions Total", width => 25},
            #{content => Sessions, width => 8}
        ],
        [
            #{content => "Accounts Total", width => 25},
            #{content => RegisteredUsers, width => 8},
            #{content => "Sessions This Node", width => 25},
            #{content => SessionsThisNode, width => 8}
        ],
        [
            #{content => "MUC Rooms", width => 25},
            #{content => OnlineRoomsNumber, width => 8}
        ]
    ],
    NewState = PrevState,
    {Attrs, NewState}.

sheet_header() ->
    [
        #{title => "Virtual Host", width => 38, shortcut => "v"},
        #{title => "Accounts", width => 11, shortcut => "a"},
        #{title => "Sessions", width => 11, shortcut => "s"},
        #{title => "Rooms", width => 8, shortcut => "r"}
    ].

sheet_body(PrevState) ->
    Body = [
        begin
            RegisteredUsers = ejabberd_auth:count_users(Host),
            Sessions = ejabberd_sm:get_vh_session_number(Host),
            OnlineRoomsNumber = lists:foldl(
                fun(Host1, Acc) ->
                    Acc + mod_muc:count_online_rooms(Host1)
                end,
                0,
                mod_muc_admin:find_hosts(Host)
            ),
            [
                Host,
                RegisteredUsers,
                Sessions,
                OnlineRoomsNumber
            ]
        end
     || Host <- lists:reverse(lists:sort(ejabberd_option:hosts()))
    ],
    NewState = PrevState,
    {Body, NewState}.
