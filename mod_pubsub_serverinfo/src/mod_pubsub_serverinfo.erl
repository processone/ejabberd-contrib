%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_serverinfo.erl
%%% Author  : Guus der Kinderen <guus.der.kinderen@gmail.com>
%%% Purpose : Exposes server information over Pub/Sub
%%% Created : 26 Dec 2023 by Guus der Kinderen <guus.der.kinderen@gmail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2023   ProcessOne
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

-module(mod_pubsub_serverinfo).
-author('guus.der.kinderen@gmail.com').
-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, depends/2, mod_options/1, get_sm_features/5, mod_doc/0,]).

-define(NS_SERVERINFO, <<"urn:xmpp:serverinfo:0">>).

start(_Host, _Opts) ->
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ok.

stop(_Host) ->
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ok.

depends(_Host, _Opts) ->
    [].

mod_options(_Host) ->
    [].

mod_doc() -> #{}.

get_sm_features({error, _} = Acc, _From, _To, _Node,
		_Lang) ->
    Acc;

get_sm_features(Acc, _From, _To, Node, _Lang) ->
    case Node of
      [] ->
	  case Acc of
	    {result, Features} ->
		{result, [?NS_SERVERINFO | Features]};
	    empty -> {result, [?NS_SERVERINFO]}
	  end;
      _ -> Acc
    end.
