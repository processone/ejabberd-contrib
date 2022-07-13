-module(ejabberd_observer_cli_userstophost).

%% observer_cli_plugin Callback API
-export([attributes/1, sheet_header/0, sheet_body/1]).

get_top_host() ->
    lists:foldl(
        fun(Host, {HostRelativeMax, CountRelativeMax}) ->
            case ejabberd_auth:count_users(Host) of
                Count when Count > CountRelativeMax ->
                    {Host, Count};
                _ ->
                    {HostRelativeMax, CountRelativeMax}
            end
        end,
        {unknown, -1},
        ejabberd_option:hosts()
    ).

attributes(PrevState) ->
    {Host, _} = get_top_host(),
    RegisteredUsers = ejabberd_auth:count_users(Host),
    Sessions = length(ejabberd_sm:dirty_get_sessions_list()),
    SessionsThisNode = length(ejabberd_sm:dirty_get_my_sessions_list()),

    Attrs = [
        [
            #{content => "Virtual Host", width => 12},
            #{content => Host, width => 14},
            #{content => "Sessions Total", width => 18},
            #{content => Sessions, width => 8}
        ],
        [
            #{content => "Accounts Total", width => 12},
            #{content => RegisteredUsers, width => 14},
            #{content => "Sessions This Node", width => 18},
            #{content => SessionsThisNode, width => 8}
        ]
    ],
    NewState = PrevState,
    {Attrs, NewState}.

sheet_header() ->
    [
        #{title => "Username", width => 25, shortcut => "u"},
        #{title => "Resources", width => 15, shortcut => "r"}
    ].

sheet_body(PrevState) ->
    {Host, _} = get_top_host(),
    Body = [
        begin
            [
                Username,
                length(ejabberd_sm:get_user_resources(Username, Host))
            ]
        end
     || {Username, _} <- lists:reverse(lists:sort(ejabberd_auth:get_users(Host)))
    ],
    NewState = PrevState,
    {Body, NewState}.
