-module(mod_autoreload).

-behaviour(gen_mod).

%% Required by ?INFO_MSG macros
-include("logger.hrl").

%% Required by ?T macro
-include("translate.hrl").

%% gen_mod API callbacks
-export([start/2, stop/1, reload/3, depends/2, mod_opt_type/1, mod_options/1, mod_doc/0]).

start(_Host, Opts) ->
    WatchSSL    = maps:get(watch_ssl, Opts, true),
    WatchConfig = maps:get(watch_config, Opts, false),
    ensure_watcher_started(WatchSSL, WatchConfig),
    ok.

reload(_Host, NewOpts, _OldOpts) ->
    WatchSSL    = maps:get(watch_ssl, NewOpts, true),
    WatchConfig = maps:get(watch_config, NewOpts, false),
    gen_server:cast(mod_autoreload_watcher, {update_settings, WatchSSL, WatchConfig}),
    ok.
    
stop(_Host) ->
    ok.

depends(_Host, _Opts) ->
    [].

mod_options(_Host) ->
    [
        {watch_ssl, true},
        {watch_config, false}
    ].

mod_doc() ->
    #{desc =>
          ?T("This is an example module.")}.

ensure_watcher_started(WatchSSL, WatchConfig) ->
    case whereis(mod_autoreload_watcher) of
    undefined ->
        mod_autoreload_watcher:start_link(WatchSSL, WatchConfig);
    _Pid ->
        ok
    end.

mod_opt_type(watch_ssl) ->
    econf:bool();
mod_opt_type(watch_config) ->
    econf:bool().