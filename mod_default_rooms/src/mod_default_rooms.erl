%%%----------------------------------------------------------------------
%%% File    : mod_default_rooms.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Auto-bookmark rooms on registration
%%% Created : 27 Feb 2019 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2019-2020   ProcessOne
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

-module(mod_default_rooms).
-author('holger@zedat.fu-berlin.de').
-behavior(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, mod_opt_type/1, depends/2, mod_options/1]).

%% ejabberd_hooks callbacks.
-export([register_user/2]).

-include("logger.hrl").
-include("xmpp.hrl").

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, _Opts) ->
    ejabberd_hooks:add(register_user, Host, ?MODULE, register_user, 50).

-spec stop(binary()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(register_user, Host, ?MODULE, register_user, 50).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(_Host, _NewOpts, _OldOpts) ->
    ok.

-spec mod_opt_type(atom()) -> fun((term()) -> term()).
mod_opt_type(auto_join) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(rooms) ->
    fun (Rs) -> [jid:decode(iolist_to_binary(R)) || R <- Rs] end.

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(_Host) ->
    [{auto_join, true},
     {rooms, []}].

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [{mod_private, hard}].

%%--------------------------------------------------------------------
%% ejabberd_hooks callbacks.
%%--------------------------------------------------------------------
-spec register_user(binary(), binary()) -> ok | {error, _}.
register_user(LUser, LServer) ->
    JID = jid:make(LUser, LServer),
    Rooms = gen_mod:get_module_opt(LServer, ?MODULE, rooms),
    AutoJoin = gen_mod:get_module_opt(LServer, ?MODULE, auto_join),
    Bookmarks = [build_bookmark(Room, AutoJoin) || Room <- Rooms],
    BookmarkStorage = #bookmark_storage{conference = Bookmarks},
    Data = [{?NS_STORAGE_BOOKMARKS, xmpp:encode(BookmarkStorage)}],
    ?DEBUG("Auto-creating bookmarks for ~s@~s", [LUser, LServer]),
    mod_private:set_data(JID, Data).

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec build_bookmark(jid(), boolean()) -> bookmark_storage().
build_bookmark(Room, AutoJoin) ->
    #bookmark_conference{jid = Room, autojoin = AutoJoin}.
