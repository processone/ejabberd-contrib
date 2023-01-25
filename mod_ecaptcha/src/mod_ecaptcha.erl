%%%----------------------------------------------------------------------
%%% File    : mod_ecaptcha.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Generate CAPTCHAs using ecaptcha
%%% Created :
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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

-module(mod_ecaptcha).
-author('badlop@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_opt_type/1, mod_doc/0]).
-export([create_image/1]).

%% ---------------------
%% gen_mod
%% ---------------------

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

depends(_Host, _Opts) ->
    [].

mod_opt_type(numchars) ->
    econf:int(1, 7);

mod_opt_type(effects) ->
    econf:list(econf:enum([line, blur, filter, dots, reverse_dots]));

mod_opt_type(color) ->
    econf:atom();

mod_opt_type(alphabet) ->
    econf:binary();

mod_opt_type(font) ->
    econf:binary().

mod_options(_Host) ->
    [{numchars, 5},
     {effects, [line, blur, filter, dots, reverse_dots]},
     {color, black},
     {alphabet, <<"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ">>},
     {font, <<"hplhs-oldstyle">>}
    ].

mod_doc() ->
    #{}.

%% ---------------------
%% CAPTCHA generation
%% ---------------------

create_image(_Key) ->
    Response = ecaptcha:png(
                 get_opt(numchars),
                 #{effects => get_opt(effects),
                   color => get_opt(color),
                   alphabet => get_opt(alphabet),
                   font => get_opt(font)}
                ),
    case Response of
        {Phrase, PNGImage} when is_binary(Phrase) ->
            PNGBin = binary:list_to_bin([PNGImage]),
            {ok, <<"image/png">>, Phrase, PNGBin};
        {error, Reason} ->
            {error, Reason}
    end.

get_opt(OptionName) ->
    Host = hd(ejabberd_option:hosts()),
    gen_mod:get_module_opt(Host, ?MODULE, OptionName).
