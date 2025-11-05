%%%----------------------------------------------------------------------
%%% File    : mod_tombstones.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Keep graveyard of accounts and rooms tombstones
%%% Created : 17 Oct 2024 by Badlop <badlop@process-one.net>
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
%%%----------------------------------------------------------------------

%%%% Definitions

%% @format-begin

-module(mod_tombstones).

-author('badlop@process-one.net').

-behaviour(gen_mod).

%% gen_mod
-export([start/2, stop/1, reload/3, depends/2, mod_opt_type/1, mod_options/1, mod_doc/0]).
%% time
-export([get_timestamp/0, is_expired/4, is_expired/3]).
%% users
-export([remove_user/2, check_register_user/4]).
%% rooms
-export([room_destroyed/4, check_create_room/4]).
%% nickmuc
-export([registering_nickmuc/5, registered_nickmuc/5]).
%% commands
-export([get_commands_spec/0, has_tombstone_command/2, delete_tombstone/2,
         delete_expired_tombstones_v0/1, delete_expired_tombstones_v3/1,
         list_tombstones_command_v0/2, list_tombstones_command_v3/2]).
%% webadmin
-export([webadmin_menu/3, webadmin_page/3]).

-import(ejabberd_web_admin,
        [make_command/4, make_command_raw_value/3, make_table/2, make_table/4]).

-callback init() -> any().
-callback set_tombstone(binary(), binary(), binary(), integer()) -> any().
-callback get_tombstone(binary(), binary()) ->
                           false | {binary(), binary(), binary(), integer()}.
-callback remove_tombstone(binary(), binary()) -> any().
-callback list_tombstones(binary(), binary()) -> [binary()].
-callback remove_expired_tombstones(binary(), integer(), atom()) -> [binary()].

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_commands.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("logger.hrl").
-include("translate.hrl").

%%%==================================
%%%% gen_mod

start(Host, _Opts) ->
    prepare_graveyard(Host),
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:register_commands(?MODULE, get_commands_spec());
        true ->
            ok
    end,
    {ok,
     [{hook, remove_user, remove_user, 50},
      {hook, check_register_user, check_register_user, 50},
      {hook, room_destroyed, room_destroyed, 50},
      {hook, check_create_room, check_create_room, 50},
      {hook, registering_nickmuc, registering_nickmuc, 50},
      {hook, registered_nickmuc, registered_nickmuc, 50},
      {hook, webadmin_menu_host, webadmin_menu, 70},
      {hook, webadmin_page_host, webadmin_page, 50}]}.

stop(Host) ->
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:unregister_commands(get_commands_spec());
        true ->
            ok
    end.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

mod_opt_type(db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(nickmuc_tombstone_expiry) ->
    econf:timeout(second, infinity);
mod_opt_type(room_tombstone_expiry) ->
    econf:timeout(second, infinity);
mod_opt_type(user_tombstone_expiry) ->
    econf:timeout(second, infinity).

mod_options(Host) ->
    [{db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {nickmuc_tombstone_expiry, 30 * 24 * 60 * 60 * 1000},
     {room_tombstone_expiry, 30 * 24 * 60 * 60 * 1000},
     {user_tombstone_expiry, 365 * 24 * 60 * 60 * 1000}].

mod_doc() ->
    #{desc =>
          [?T("Keep tombstones for accounts and rooms that were removed. "
              "This prevents registration of that account, and creation of that room.")],
      note => "added in 24.xx",
      opts =>
          [{db_type,
            #{value => "mnesia | sql | ldap",
              desc =>
                  ?T("Same as top-level _`default_db`_ option, but applied to this module only.")}},
           {nickmuc_tombstone_expiry,
            #{value => ?T("time | infinity"),
              desc =>
                  ?T("How long to keep tombstones of nick registrations in MUC services. "
                     "If set to 'infinity' the tombstones are forever. "
                     "The default value is '30 days'.")}},
           {room_tombstone_expiry,
            #{value => ?T("time | infinity"),
              desc =>
                  ?T("How long to keep MUC room tombstones. "
                     "If set to 'infinity' the tombstones are forever. "
                     "The default value is '30 days'.")}},
           {user_tombstone_expiry,
            #{value => ?T("time | infinity"),
              desc =>
                  ?T("How long to keep users tombstones, for example '365 days'. "
                     "If set to 'infinity' the tombstones are forever. "
                     "The default value is '365 days'.")}}]}.

%%%==================================
%%%% Tombstones

prepare_graveyard(Host) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:init().

create_tombstone(Vhost, N, H, V) ->
    Timestamp = get_timestamp(),
    Mod = gen_mod:db_mod(Vhost, ?MODULE),
    Mod:set_tombstone(N, H, V, Timestamp).

has_tombstone(Vhost, N, H) ->
    Mod = gen_mod:db_mod(Vhost, ?MODULE),
    case Mod:get_tombstone(N, H) of
        false ->
            false;
        {_, _, _VStored, Timestamp} ->
            case is_expired(Vhost, N, H, Timestamp) of
                true ->
                    delete_tombstone(Vhost, N, H),
                    false;
                false ->
                    true
            end
    end.

has_tombstone_value(Vhost, N, H, V) ->
    Mod = gen_mod:db_mod(Vhost, ?MODULE),
    case Mod:get_tombstone(N, H) of
        false ->
            false;
        {_, _, VStored, Timestamp} ->
            case is_expired(Vhost, N, H, Timestamp) of
                true ->
                    delete_tombstone(Vhost, N, H),
                    false;
                false ->
                    case V == VStored of
                        true ->
                            same_value;
                        false ->
                            true
                    end
            end
    end.

delete_tombstone(Name, Host) ->
    Vhost = get_server_host(Host),
    Mod = gen_mod:db_mod(Vhost, ?MODULE),
    Mod:remove_tombstone(Name, Host).

delete_tombstone(Vhost, Name, Host) ->
    Mod = gen_mod:db_mod(Vhost, ?MODULE),
    Mod:remove_tombstone(Name, Host).

delete_expired_tombstones_v0(Vhost) ->
    Result = delete_expired_tombstones_v3(Vhost),
    [jid:encode(
         jid:make(N, H))
     || {N, H} <- Result].

delete_expired_tombstones_v3(Vhost) ->
    Mod = gen_mod:db_mod(Vhost, ?MODULE),
    ExpiryUsers = get_expiry(Vhost, users),
    Users = Mod:remove_expired_tombstones(Vhost, ExpiryUsers, users),
    RoomsNicks =
        case mod_muc_admin:find_hosts(Vhost) of
            [] ->
                [];
            [MucHost] ->
                ExpiryRooms = get_expiry(Vhost, room),
                Rooms = Mod:remove_expired_tombstones(MucHost, ExpiryRooms, room),
                ExpiryNick = get_expiry(Vhost, nickmuc),
                Nicks = Mod:remove_expired_tombstones(MucHost, ExpiryNick, nickmuc),
                Rooms ++ Nicks
        end,
    Users ++ RoomsNicks.

list_tombstones(Vhost, Type) ->
    Mod = gen_mod:db_mod(Vhost, ?MODULE),
    Mod:list_tombstones(Vhost, Type).

%%%==================================
%%%% Time

get_timestamp() ->
    erlang:system_time(second).

%% TODO +++
get_expiry(Host, Type) ->
    ExpiryOption =
        case Type of
            nickmuc ->
                nickmuc_tombstone_expiry;
            room ->
                room_tombstone_expiry;
            users ->
                user_tombstone_expiry
        end,
    gen_mod:get_module_opt(Host, mod_tombstones, ExpiryOption).

%% TODO +++
is_expired(Vhost, Name, Host, Timestamp) ->
    ExpiryOption =
        case {Vhost == Host, Name} of
            {true, _} ->
                user_tombstone_expiry;
            {false, N} when is_binary(N) and N /= <<>> ->
                nickmuc_tombstone_expiry;
            {false, _} ->
                room_tombstone_expiry
        end,
    %% Expiry = mod_tombstones_opt:user_tombstone_expiry(Vhost),
    Expiry = gen_mod:get_module_opt(Vhost, ?MODULE, ExpiryOption),
    CurrentTimestamp = get_timestamp(),
    is_expired(Timestamp, Expiry, CurrentTimestamp).

is_expired(_Timestamp, infinity, _CurrentTimestamp) ->
    false;
is_expired(Timestamp, Expiry, CurrentTimestamp) ->
    Timestamp + Expiry div 1000 < CurrentTimestamp.

timestamp_to_string(TS) ->
    xmpp_util:encode_timestamp({TS div 1000000, TS rem 1000000, 0}).

%%%==================================
%%%% Users: {username, host}, ""

-spec remove_user(binary(), binary()) -> ok.
remove_user(Name, Host) ->
    case has_tombstone(Host, Name, Host) of
        false ->
            ok = create_tombstone(Host, Name, Host, <<"">>),
            ok;
        true ->
            ok
    end.

-spec check_register_user(boolean(), binary(), binary(), binary()) -> boolean().
check_register_user(Acc, Name, Host, _Password) ->
    Acc and not has_tombstone(Host, Name, Host).

%%%==================================
%%%% Rooms: {roomname, muchost}, ""

-spec room_destroyed(binary(), binary(), binary(), boolean()) -> ok.
room_destroyed(LServer, Room, Host, true) ->
    case has_tombstone(LServer, Room, Host) of
        false ->
            ok = create_tombstone(LServer, Room, Host, <<"">>),
            ok;
        true ->
            ok
    end;
room_destroyed(_LServer, _Room, _Host, _Persistent) ->
    ok.

-spec check_create_room(boolean(), binary(), binary(), binary()) -> boolean().
check_create_room(Acc, ServerHost, RoomID, Host) ->
    Acc and not has_tombstone(ServerHost, RoomID, Host).

get_server_host(Host) ->
    case lists:member(Host, ejabberd_option:hosts()) of
        true ->
            Host;
        false ->
            mod_muc_admin:get_room_serverhost(Host)
    end.

%%%==================================
%%%% Nick MUC: {nick, muchost}, user@host

-spec registering_nickmuc(boolean(), binary(), binary(), jid(), binary()) -> boolean().
registering_nickmuc(Acc, _ServerHost, _MucHost, _Account, <<"">>) ->
    Acc;
registering_nickmuc(Acc, ServerHost, MucHost, Account, NewNick) ->
    case has_tombstone_value(ServerHost, NewNick, MucHost, jid_bare_string(Account)) of
        same_value ->
            delete_tombstone(ServerHost, NewNick, MucHost),
            Acc;
        HasTombstone ->
            Acc and not HasTombstone
    end.

-spec registered_nickmuc(binary(), binary(), jid(), binary(), binary()) -> ok.
registered_nickmuc(_ServerHost, _MucHost, _Account, SameNick, SameNick) ->
    ok;
registered_nickmuc(_ServerHost, _MucHost, _Account, _NewNick, <<"">>) ->
    ok;
registered_nickmuc(ServerHost, MucHost, Account, _NewNick, OldNick) ->
    ok = create_tombstone(ServerHost, OldNick, MucHost, jid_bare_string(Account)).

jid_bare_string(Jid) ->
    jid:encode(
        jid:remove_resource(Jid)).

%%%==================================
%%%% Commands

get_commands_spec() ->
    [#ejabberd_commands{name = has_tombstone,
                        tags = [tombstone],
                        desc = "Check if there's an active tombstone",
                        module = ?MODULE,
                        function = has_tombstone_command,
                        policy = user,
                        args = [],
                        result = {res, rescode}},
     #ejabberd_commands{name = has_tombstone,
                        tags = [tombstone],
                        desc = "Check if there's an active tombstone",
                        module = ?MODULE,
                        function = has_tombstone_command,
                        version = 3,
                        args = [{name, binary}, {host, binary}],
                        result = {res, rescode}},
     #ejabberd_commands{name = delete_tombstone,
                        tags = [tombstone],
                        desc = "Delete this tombstone",
                        module = ?MODULE,
                        function = delete_tombstone,
                        policy = user,
                        args = [],
                        result = {res, rescode}},
     #ejabberd_commands{name = delete_tombstone,
                        tags = [tombstone],
                        desc = "Delete this tombstone",
                        module = ?MODULE,
                        function = delete_tombstone,
                        version = 3,
                        args = [{name, binary}, {host, binary}],
                        result = {res, rescode}},
     #ejabberd_commands{name = delete_expired_tombstones,
                        tags = [tombstone],
                        desc = "Delete expired tombstones",
                        module = ?MODULE,
                        function = delete_expired_tombstones_v0,
                        args = [{host, binary}],
                        result = {tombstones, {list, {jid, string}}}},
     #ejabberd_commands{name = delete_expired_tombstones,
                        tags = [tombstone],
                        desc = "Delete expired tombstones",
                        module = ?MODULE,
                        function = delete_expired_tombstones_v3,
                        version = 3,
                        args = [{host, binary}],
                        result =
                            {tombstones,
                             {list, {tombstone, {tuple, [{name, string}, {host, string}]}}}}},
     #ejabberd_commands{name = list_tombstones,
                        tags = [tombstone],
                        desc = "List tombstones in the graveyard",
                        module = ?MODULE,
                        function = list_tombstones_command_v0,
                        args = [{host, binary}, {type, binary}],
                        args_example = [<<"myserver.com">>, <<"all">>],
                        args_desc =
                            ["Server name", "Type of tombstones to list: all | rooms | users"],
                        result =
                            {tombstones,
                             {list, {tombstone, {tuple, [{jid, string}, {timestamp, string}]}}}}},
     #ejabberd_commands{name = list_tombstones,
                        tags = [tombstone],
                        desc = "List tombstones in the graveyard",
                        module = ?MODULE,
                        function = list_tombstones_command_v3,
                        version = 3,
                        args = [{host, binary}, {type, binary}],
                        args_example = [<<"myserver.com">>, <<"all">>],
                        args_desc =
                            ["Server name",
                             "Type of tombstones to list: all | nickmuc | rooms | users"],
                        result =
                            {tombstones,
                             {list,
                              {tombstone,
                               {tuple,
                                [{name, string},
                                 {host, string},
                                 {value, string},
                                 {timestamp, string}]}}}}}].

has_tombstone_command(Username, Host) ->
    has_tombstone(Host, Username, Host).

list_tombstones_command_v0(Host, Type) ->
    [{jid:encode(
          jid:make(Username, H)),
      timestamp_to_string(Timestamp)}
     || [Username, H, Timestamp] <- list_tombstones(Host, Type)].

list_tombstones_command_v3(Host, Type) ->
    lists:map(fun([N, H, V, Timestamp]) -> {N, H, V, timestamp_to_string(Timestamp)} end,
              list_tombstones(Host, Type)).

%%%==================================
%%%% WebAdmin

webadmin_menu(Acc, _Host, Lang) ->
    [{<<"tombstones">>, translate:translate(Lang, ?T("Tombstones"))} | Acc].

webadmin_page(_,
              Host,
              #request{us = _US,
                       path = [<<"tombstones">>, <<"nickmuc">> | _RPath],
                       lang = Lang} =
                  R) ->
    PageTitle = translate:translate(Lang, ?T("Tombstones: Nick MUC")),
    Head = ?H1GL(PageTitle, <<"modules/#mod_tombstones">>, <<"mod_tombstones">>),
    Set = case mod_muc_admin:find_hosts(Host) of
              [] ->
                  [];
              [MucHost] ->
                  [make_command(delete_tombstone, R, [{<<"host">>, MucHost}], [{style, danger}])]
          end,
    Get = [make_command(list_tombstones,
                        R,
                        [{<<"host">>, Host}, {<<"type">>, <<"nickmuc">>}],
                        [])],
    {stop, Head ++ Get ++ Set};
webadmin_page(_,
              Host,
              #request{us = _US,
                       path = [<<"tombstones">>, <<"users">> | _RPath],
                       lang = Lang} =
                  R) ->
    PageTitle = translate:translate(Lang, ?T("Tombstones: Users")),
    Head = ?H1GL(PageTitle, <<"modules/#mod_tombstones">>, <<"mod_tombstones">>),
    Set = [make_command(delete_tombstone, R, [{<<"host">>, Host}], [{style, danger}])],
    Get = [make_command(list_tombstones,
                        R,
                        [{<<"host">>, Host}, {<<"type">>, <<"users">>}],
                        [])],
    {stop, Head ++ Get ++ Set};
webadmin_page(_,
              Host,
              #request{us = _US,
                       path = [<<"tombstones">>, <<"rooms">> | _RPath],
                       lang = Lang} =
                  R) ->
    PageTitle = translate:translate(Lang, ?T("Tombstones: Rooms")),
    Head = ?H1GL(PageTitle, <<"modules/#mod_tombstones">>, <<"mod_tombstones">>),
    Set = case mod_muc_admin:find_hosts(Host) of
              [] ->
                  [];
              [MucHost] ->
                  [make_command(delete_tombstone, R, [{<<"host">>, MucHost}], [{style, danger}])]
          end,
    Get = [make_command(list_tombstones,
                        R,
                        [{<<"host">>, Host}, {<<"type">>, <<"rooms">>}],
                        [])],
    {stop, Head ++ Get ++ Set};
webadmin_page(_,
              Host,
              #request{us = _US,
                       path = [<<"tombstones">> | _RPath],
                       lang = Lang} =
                  R) ->
    PageTitle = translate:translate(Lang, ?T("Tombstones")),
    Head = ?H1GL(PageTitle, <<"modules/#mod_tombstones">>, <<"mod_tombstones">>),
    Types =
        [{<<"users">>, <<"Users">>}, {<<"rooms">>, <<"Rooms">>}, {<<"nickmuc">>, <<"Nick MUC">>}],
    Links = [?XE(<<"ul">>, [?LI([?AC(MIU, MIN)]) || {MIU, MIN} <- Types])],
    Set = [make_command(delete_expired_tombstones,
                        R,
                        [{<<"host">>, Host}],
                        [{style, danger}, {force_execution, false}])],
    Get = [make_command(list_tombstones,
                        R,
                        [{<<"host">>, Host}, {<<"type">>, <<"all">>}],
                        [])],
    {stop, Head ++ Links ++ Get ++ Set};
webadmin_page(Acc, _, _) ->
    Acc.
%%% @format-end
%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
