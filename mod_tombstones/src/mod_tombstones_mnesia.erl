%%%----------------------------------------------------------------------
%%% File    : mod_tombstones_mnesia.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose :
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

%% @format-begin

-module(mod_tombstones_mnesia).

-behaviour(mod_tombstones).

-export([init/0, set_tombstone/3, get_tombstone/2, remove_tombstone/2, list_tombstones/2,
         remove_expired_tombstones/2]).

-define(GRAVEYARD, mod_tombstones_graveyard).

-record(?GRAVEYARD, {key :: {binary(), binary()}, timestamp :: integer()}).

init() ->
    ejabberd_mnesia:create(?MODULE,
                           ?GRAVEYARD,
                           [{disc_copies, [node()]},
                            {attributes, record_info(fields, ?GRAVEYARD)}]).

set_tombstone(U, H, Timestamp) ->
    mnesia:dirty_write({?GRAVEYARD, {U, H}, Timestamp}).

get_tombstone(U, H) ->
    case mnesia:dirty_read(?GRAVEYARD, {U, H}) of
        [] ->
            false;
        [{_, _, Timestamp}] ->
            {U, H, Timestamp}
    end.

remove_tombstone(U, H) ->
    mnesia:dirty_delete(?GRAVEYARD, {U, H}).

list_tombstones(Host, <<"rooms">>) ->
    case mod_muc_admin:find_hosts(Host) of
        [] ->
            [];
        [MucHost] ->
            mnesia:dirty_select(?GRAVEYARD,
                                [{{?GRAVEYARD, {'$1', MucHost}, '$2'},
                                  [],
                                  [['$1', MucHost, '$2']]}])
    end;
list_tombstones(Host, <<"users">>) ->
    mnesia:dirty_select(?GRAVEYARD,
                        [{{?GRAVEYARD, {'$1', Host}, '$2'}, [], [['$1', Host, '$2']]}]);
list_tombstones(Host, <<"all">>) ->
    list_tombstones(Host, <<"rooms">>) ++ list_tombstones(Host, <<"users">>).

remove_expired_tombstones(Host, Expiry) ->
    CurrentTS = mod_tombstones:get_timestamp(),
    F = fun() ->
           mnesia:write_lock_table(?GRAVEYARD),
           mnesia:foldl(fun(#?GRAVEYARD{key = {U, H}, timestamp = TS}, Acc) ->
                           case Host == H
                                andalso mod_tombstones:is_expired(H, TS, Expiry, CurrentTS)
                           of
                               true ->
                                   remove_tombstone(U, H),
                                   [jid:encode(
                                        jid:make(U, H))
                                    | Acc];
                               false ->
                                   Acc
                           end
                        end,
                        [],
                        ?GRAVEYARD)
        end,
    {atomic, DeletedTombstones} = mnesia:transaction(F),
    DeletedTombstones.
%% @format-end
