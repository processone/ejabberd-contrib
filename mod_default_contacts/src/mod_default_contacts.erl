%%%----------------------------------------------------------------------
%%% File    : mod_default_contacts.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Auto-add contacts on registration
%%% Created : 14 May 2019 by Holger Weiss <holger@zedat.fu-berlin.de>
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

-module(mod_default_contacts).
-author('holger@zedat.fu-berlin.de').
-behavior(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, mod_opt_type/1, depends/2, mod_options/1,
        mod_doc/0]).

%% ejabberd_hooks callbacks.
-export([register_user/2]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

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
mod_opt_type(contacts) ->
    fun (L) ->
	    lists:map(fun (Opts) ->
			      JID1 = proplists:get_value(jid, Opts),
			      JID2 = iolist_to_binary(JID1),
			      JID3 = jid:decode(JID2),
			      Name1 = proplists:get_value(name, Opts, <<>>),
			      Name2 = iolist_to_binary(Name1),
			      #roster_item{jid = JID3, name = Name2}
		      end, L)
    end.

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(_Host) ->
    [{contacts, []}].

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [{mod_roster, hard}].

mod_doc() ->
    #{}.

%%--------------------------------------------------------------------
%% ejabberd_hooks callbacks.
%%--------------------------------------------------------------------
-spec register_user(binary(), binary()) -> any().
register_user(LUser, LServer) ->
    ?DEBUG("Auto-creating roster entries for ~s@~s", [LUser, LServer]),
    Items = gen_mod:get_module_opt(LServer, ?MODULE, contacts),
    mod_roster:set_items(LUser, LServer, #roster_query{items = Items}).
