-module(ejabberd_observer_cli_muc).

%% observer_cli_plugin Callback API
-export([attributes/1, sheet_header/0, sheet_body/1]).

attributes(PrevState) ->
    OnlineRoomsNumber = lists:foldl(
        fun(Host, Acc) ->
            Acc + mod_muc:count_online_rooms(Host)
        end,
        0,
        mod_muc_admin:find_hosts(global)
    ),

    Attrs = [
        [
            #{content => "MUC Rooms", width => 25},
            #{content => OnlineRoomsNumber, width => 8}
        ]
    ],
    NewState = PrevState,
    {Attrs, NewState}.

sheet_header() ->
    [
        #{title => "Room Name", width => 20, shortcut => "n"},
        #{title => "MUC Service", width => 30, shortcut => "s"},
        #{title => "Occupants", width => 12, shortcut => "o"},
        #{title => "Subscribers", width => 13, shortcut => "r"}
    ].

sheet_body(PrevState) ->
    Body = [
        begin
            {Name, Service, _} = jid:split(jid:decode(RoomStr)),
            OccupantsNumber = mod_muc_admin:get_room_occupants_number(Name, Service),
            SubsNumber = length(mod_muc_admin:get_subscribers(Name, Service)),
            [
                Name,
                Service,
                OccupantsNumber,
                SubsNumber
            ]
        end
     || RoomStr <- mod_muc_admin:muc_online_rooms(global)
    ],
    NewState = PrevState,
    {Body, NewState}.
