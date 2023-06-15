%%%-------------------------------------------------------------------
%%% File    : mod_http_redirect.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Redirect HTTP path to another URI
%%% Created : 15 May 2023 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-module(mod_http_redirect).

-author('badlop@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process/2]).
-export([mod_opt_type/1, mod_options/1, mod_doc/0, mod_status/0, depends/2]).

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_http.hrl").

-include("translate.hrl").

%%%----------------------------------------------------------------------
%%% gen_mod callbacks
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    ok.

stop(_Host) -> ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

%%%----------------------------------------------------------------------
%%% HTTP handlers
%%%----------------------------------------------------------------------

process(_Path, #request{host = Host}) ->
    try gen_mod:get_module_opt(Host, ?MODULE, location) of
        Location when is_binary(Location) ->
            {301, [{<<"Location">>, Location}], <<>>}
    catch
        error:{module_not_loaded, ?MODULE, Host} ->
            {404, [], <<"Not Found">>}
    end.

%%%----------------------------------------------------------------------
%%% Options and documentation
%%%----------------------------------------------------------------------

mod_opt_type(location) ->
    econf:binary().

mod_options(_) ->
    [{location, <<"">>}].

mod_doc() -> #{}.

mod_status() ->
    "".
