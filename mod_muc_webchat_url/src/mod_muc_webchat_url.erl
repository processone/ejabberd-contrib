%%%-------------------------------------------------------------------
%%% File    : mod_muc_webchat_url.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Show webchat_url in MUC rooms disco#info
%%% Created : 18 Mar 2025 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
%%%-------------------------------------------------------------------

%%%% definitions

%% @format-begin

-module(mod_muc_webchat_url).

-author('badlop@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, mod_opt_type/1, mod_options/1, depends/2, mod_doc/0]).
-export([disco_info_extras/2]).

-include_lib("xmpp/include/xmpp.hrl").

-include("logger.hrl").
-include("mod_muc_room.hrl").
-include("translate.hrl").

%%%==================================
%%%% gen_mod

start(Host, _Opts) ->
    BaseUrl = mod_muc_webchat_url_opt:base_url(Host),
    AutoUrl = get_auto_url(any, mod_conversejs, Host),
    case {BaseUrl, AutoUrl} of
        {auto, undefined} ->
            ?CRITICAL_MSG("Module ~p is enabled for host ~s, "
                          "base_url is configured as auto... "
                          "but couldn't find any mod_conversejs listener!",
                          [?MODULE, Host]),
            {error, mod_converse_not_listening};
        _ ->
            {ok, [{hook, muc_disco_info_extras, disco_info_extras, 50}]}
    end.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [{mod_conversejs, soft}].

mod_opt_type(base_url) ->
    econf:either(auto, econf:binary());
mod_opt_type(room_names) ->
    econf:list(
        econf:binary());
mod_opt_type(room_options) ->
    econf:map(
        econf:atom(), econf:bool()).

mod_options(_) ->
    [{base_url, auto},
     {room_names, []},
     {room_options, [{members_only, false}, {password_protected, false}, {public, true}]}].

mod_doc() ->
    #{desc => [?T("This small module shows webchat_url in MUC rooms disco#info.")]}.

%%%==================================
%%%% hooks

disco_info_extras(Acc0,
                  #state{room = RoomName,
                         server_host = ServerHost,
                         config = Config} =
                      State) ->
    Names = mod_muc_webchat_url_opt:room_names(ServerHost),
    IsNamed = lists:member(RoomName, Names),

    Options = mod_muc_webchat_url_opt:room_options(ServerHost),
    IsOptions = is_options(Options, Config),

    case IsNamed or IsOptions of
        true ->
            BaseUrl = get_base_url(ServerHost),
            [{webchat_url, prepare_url(BaseUrl, State)} | Acc0];
        _ ->
            Acc0
    end.

is_options(Options, Config) ->
    [config | ConfigList] = tuple_to_list(Config),
    ConfigListZipped = lists:zip(record_info(fields, config), ConfigList),
    lists:all(fun({Key, Val}) -> proplists:get_bool(Key, ConfigListZipped) == Val end,
              Options).

get_base_url(ServerHost) ->
    case mod_muc_webchat_url_opt:base_url(ServerHost) of
        auto ->
            get_auto_url(any, mod_conversejs, ServerHost);
        B when is_binary(B) ->
            B
    end.

prepare_url(BaseUrl,
            #state{room = Name,
                   host = Host,
                   server_host = ServerHost,
                   jid = Jid}) ->
    Replacements =
        [{<<"ROOM_JID">>, jid:encode(Jid)}, {<<"ROOM_NAME">>, Name}, {<<"ROOM_HOST">>, Host}],
    ejabberd_config:replace_keywords(ServerHost, BaseUrl, Replacements).

%% Copied from mod_host_meta.erl and customized
get_auto_url(Tls, Module, ServerHost) ->
    case find_handler_port_path(Tls, Module) of
        [] ->
            undefined;
        [{ThisTls, Port, Path} | _] ->
            Protocol =
                case {ThisTls, Module} of
                    {false, Module} ->
                        <<"http">>;
                    {true, Module} ->
                        <<"https">>
                end,
            <<Protocol/binary,
              "://",
              ServerHost/binary,
              ":",
              (integer_to_binary(Port))/binary,
              "/",
              (str:join(Path, <<"/">>))/binary,
              "/#converse/room?jid=@ROOM_JID@">>
    end.

find_handler_port_path(Tls, Module) ->
    lists:filtermap(fun ({{Port, _, _},
                          ejabberd_http,
                          #{tls := ThisTls, request_handlers := Handlers}})
                            when is_integer(Port) and ((Tls == any) or (Tls == ThisTls)) ->
                            case lists:keyfind(Module, 2, Handlers) of
                                false ->
                                    false;
                                {Path, Module} ->
                                    {true, {ThisTls, Port, Path}}
                            end;
                        (_) ->
                            false
                    end,
                    ets:tab2list(ejabberd_listener)).

%%%==================================

%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
