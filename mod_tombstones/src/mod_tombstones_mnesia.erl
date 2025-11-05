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

-export([init/0, set_tombstone/4, get_tombstone/2, remove_tombstone/2, list_tombstones/2,
         remove_expired_tombstones/3]).

-define(GRAVEYARD, mod_tombstones_graveyard).

-record(?GRAVEYARD,
        {key :: {binary(), binary()}, value :: binary(), timestamp :: integer()}).

init() ->
    ejabberd_mnesia:create(?MODULE,
                           ?GRAVEYARD,
                           [{disc_copies, [node()]},
                            {attributes, record_info(fields, ?GRAVEYARD)}]).

set_tombstone(N, H, V, Timestamp) ->
    mnesia:dirty_write({?GRAVEYARD, {N, H}, V, Timestamp}).

get_tombstone(N, H) ->
    case mnesia:dirty_read(?GRAVEYARD, {N, H}) of
        [] ->
            false;
        [{?GRAVEYARD, {N, H}, V, Timestamp}] ->
            {N, H, V, Timestamp}
    end.

remove_tombstone(N, H) ->
    mnesia:dirty_delete(?GRAVEYARD, {N, H}).

list_tombstones(Host, <<"nickmuc">>) ->
    case mod_muc_admin:find_hosts(Host) of
        [] ->
            [];
        [MucHost] ->
            mnesia:dirty_select(?GRAVEYARD,
                                [{{?GRAVEYARD, {'$1', MucHost}, '$2', '$3'},
                                  [{'/=', '$2', <<>>}],
                                  [['$1', MucHost, '$2', '$3']]}])
    end;
list_tombstones(Host, <<"rooms">>) ->
    case mod_muc_admin:find_hosts(Host) of
        [] ->
            [];
        [MucHost] ->
            mnesia:dirty_select(?GRAVEYARD,
                                [{{?GRAVEYARD, {'$1', MucHost}, '$2', '$3'},
                                  [{'==', '$2', <<>>}],
                                  [['$1', MucHost, '$2', '$3']]}])
    end;
list_tombstones(Host, <<"users">>) ->
    mnesia:dirty_select(?GRAVEYARD,
                        [{{?GRAVEYARD, {'$1', Host}, '$2', '$3'}, [], [['$1', Host, '$2', '$3']]}]);
list_tombstones(Host, <<"all">>) ->
    list_tombstones(Host, <<"nickmuc">>)
    ++ list_tombstones(Host, <<"rooms">>)
    ++ list_tombstones(Host, <<"users">>).

remove_expired_tombstones(Host, Expiry, Type) ->
    CurrentTS = mod_tombstones:get_timestamp(),
    F = fun() ->
           mnesia:write_lock_table(?GRAVEYARD),
           mnesia:foldl(fun(#?GRAVEYARD{key = {N, H},
                                        value = V,
                                        timestamp = TS},
                            Acc) ->
                           case Host == H
                                andalso mod_tombstones:is_expired(TS, Expiry, CurrentTS)
                                andalso (Type /= nickmuc) or (V /= <<>>)
                           of
                               true ->
                                   remove_tombstone(N, H),
                                   [{N, H} | Acc];
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
