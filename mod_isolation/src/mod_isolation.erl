%%%----------------------------------------------------------------------
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
-module(mod_isolation).
-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1, reload/3, mod_options/1, depends/2]).
%% hooks
-export([filter_packet/1]).

-include("xmpp.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start(_Host, _Opts) ->
    ejabberd_hooks:add(filter_packet, ?MODULE, filter_packet, 50).

stop(Host) ->
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
	false ->
	    ejabberd_hooks:delete(filter_packet, ?MODULE, filter_packet, 50);
	true ->
	    ok
    end.

reload(_, _, _) ->
    ok.

mod_options(_) ->
    [].

depends(_Host, _Opts) ->
    [].

%%%===================================================================
%%% Internal functions
%%%===================================================================
filter_packet(drop) ->
    drop;
filter_packet(Pkt) ->
    case xmpp:get_meta(Pkt, already_filtered, false) of
	true ->
	    Pkt;
	false ->
	    From = xmpp:get_from(Pkt),
	    To = xmpp:get_to(Pkt),
	    try {ejabberd_router:host_of_route(From#jid.lserver),
		 ejabberd_router:host_of_route(To#jid.lserver)} of
		{Host, Host} ->
		    Pkt;
		{_Host1, _Host2} ->
		    Pkt1 = xmpp:put_meta(Pkt, already_filtered, true),
		    Lang = xmpp:get_lang(Pkt),
		    %% We already have translations for this phrase
		    Txt = <<"Access denied by service policy">>,
		    Err = xmpp:err_forbidden(Txt, Lang),
		    ejabberd_router:route_error(Pkt1, Err),
		    {stop, drop}
	    catch _:{unregistered_route, _} ->
		    %% This will go to s2s manager
		    Pkt
	    end
    end.
