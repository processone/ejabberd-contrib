-module(ejabberd_observer_cli_users).

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

    Attrs = [
        [
            #{content => "Virtual Hosts", width => 18},
            #{content => Hosts, width => 8},
            #{content => "Sessions Total", width => 18},
            #{content => Sessions, width => 8}
        ],
        [
            #{content => "Accounts Total", width => 18},
            #{content => RegisteredUsers, width => 8},
            #{content => "Sessions This Node", width => 18},
            #{content => SessionsThisNode, width => 8}
        ]
    ],
    NewState = PrevState,
    {Attrs, NewState}.

sheet_header() ->
    [
        #{title => "Username", width => 25, shortcut => "u"},
        #{title => "Host", width => 25, shortcut => "h"},
        #{title => "Sessions", width => 11, shortcut => "s"},
        #{title => "Roster", width => 9, shortcut => "r"},
        #{title => "Offline", width => 10, shortcut => "o"},
        #{title => "Last Activity", width => 20, shortcut => "l"}
    ].

sheet_body(PrevState) ->
    Body = [
        begin
            [
                Username,
                Host,
                length(ejabberd_sm:get_user_resources(Username, Host)),
                length(mod_roster:get_roster(Username, Host)),
                mod_offline:count_offline_messages(Username, Host),
                get_last_activity(Username, Host)
            ]
        end
     || {Username, Host} <- lists:sort(ejabberd_auth:get_users())
    ],
    NewState = PrevState,
    {Body, NewState}.

%% Code copied from ejabberd_web_admin.erl
get_last_activity(User, Server) ->
    case ejabberd_sm:get_user_resources(User, Server) of
        [] ->
            case get_last_info(User, Server) of
                not_found ->
                    "Never";
                {ok, Shift, _Status} ->
                    TimeStamp = {Shift div 1000000, Shift rem 1000000, 0},
                    {{Year, Month, Day}, {Hour, Minute, Second}} =
                        calendar:now_to_local_time(TimeStamp),
                    (io_lib:format(
                        "~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                        [
                            Year,
                            Month,
                            Day,
                            Hour,
                            Minute,
                            Second
                        ]
                    ))
            end;
        _ ->
            "Online"
    end.
get_last_info(User, Server) ->
    case gen_mod:is_loaded(Server, mod_last) of
        true ->
            mod_last:get_last_info(User, Server);
        false ->
            not_found
    end.
