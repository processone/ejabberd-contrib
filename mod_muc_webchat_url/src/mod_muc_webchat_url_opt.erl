%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_muc_webchat_url_opt).

-export([base_url/1]).
-export([room_names/1]).
-export([room_options/1]).

-spec base_url(gen_mod:opts() | global | binary()) -> binary().
base_url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(base_url, Opts);
base_url(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_webchat_url, base_url).

-spec room_names(gen_mod:opts() | global | binary()) -> [binary()].
room_names(Opts) when is_map(Opts) ->
    gen_mod:get_opt(room_names, Opts);
room_names(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_webchat_url, room_names).

-spec room_options(gen_mod:opts() | global | binary()) -> [{atom(),boolean()}].
room_options(Opts) when is_map(Opts) ->
    gen_mod:get_opt(room_options, Opts);
room_options(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_webchat_url, room_options).

