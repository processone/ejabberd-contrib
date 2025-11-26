-module(mod_autoreload_watcher).
-behaviour(gen_server).

-include("logger.hrl").

-include_lib("kernel/include/file.hrl").

-export([start_link/2]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2,
         terminate/2, code_change/3, system_started/0]).

-export([config_reloaded/0]).

-define(INTERVAL, 10000).

start_link(WatchSSL, WatchConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [WatchSSL, WatchConfig], []).

init([WatchSSL, WatchConfig]) ->    
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 50),
    ejabberd_hooks:add(ejabberd_started, ?MODULE, system_started, 110),
    
    timer:send_interval(?INTERVAL, check),
    
    {ok, #{
        watch_ssl      => WatchSSL,
        watch_config   => WatchConfig,
        last_reload  => erlang:system_time(second),
        initialized => false,
        files => []
    }}.

system_started() ->
    gen_server:cast(?MODULE, system_started),
    ok.

config_reloaded() ->
    gen_server:cast(?MODULE, reload_config),
    ok.

%% Periodic check: see if any file has changed since last_reload
handle_info(check, State = #{initialized := true,
                         last_reload := LastReload,
                         files := Files}) ->
    case any_file_changed(Files, LastReload) of
        false ->
            ?DEBUG("mod_autoreload: Files NOT changed since last reload", []),
            {noreply, State};
        true ->
            ?INFO_MSG("mod_autoreload: relevant file changed, reloading config", []),
            case ejabberd_config:reload() of
                ok ->
                    ok;
                Err ->
                    Reason = ejabberd_config:format_error(Err),
                    ?ERROR_MSG("mod_autoreload: ejabberd_config:reload failed: ~s",
                               [Reason])
            end,
            Now = erlang:system_time(second),
            {noreply, State#{last_reload := Now}}
    end;


%% Ignore checks before initialization
handle_info(check, State) ->
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({update_settings, WatchSSL, WatchConfig}, State) ->
    OldSSL    = maps:get(watch_ssl, State, undefined),
    OldConfig = maps:get(watch_config, State, undefined),
    
    case {OldSSL, OldConfig} of
        {WatchSSL, WatchConfig} ->
            %% Settings did not change
            {noreply, State};
        _ ->
            %% Settings changed
            NewState0 = State#{
                watch_ssl    := WatchSSL,
                watch_config := WatchConfig
            },
    
            Files = get_files(NewState0),
    
            ?INFO_MSG("mod_autoreload: Watcher settings updated, ssl=~p config=~p", [WatchSSL, WatchConfig]),
        
            {noreply, NewState0#{
                files       := Files,
                watch_ssl    := WatchSSL,
                watch_config := WatchConfig,
                last_reload := erlang:system_time(second)
            }}
    end;

handle_cast(system_started, State) ->
    WatchSSL    = maps:get(watch_ssl, State, true),
    WatchConfig = maps:get(watch_config, State, false),
    ?INFO_MSG("mod_autoreload: Watcher started, ssl=~p config=~p", [WatchSSL, WatchConfig]),
    Files = get_files(State),    
    {noreply, State#{
        files := Files,
        last_reload  => erlang:system_time(second),
        initialized => true
    }};

handle_cast(reload_config, State) ->
    WatchSSL    = maps:get(watch_ssl, State, true),
    WatchConfig = maps:get(watch_config, State, false),
    ?INFO_MSG("mod_autoreload: Watcher reloaded, ssl=~p config=~p", [WatchSSL, WatchConfig]),
    Files = get_files(State),
    {noreply, State#{
        files => Files,
        last_reload  => erlang:system_time(second),
        initialized => true
    }};

handle_cast(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 50),
    ejabberd_hooks:delete(ejabberd_started, ?MODULE, system_started, 110),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%% Combine certfiles and configfiles based on watch_ssl / watch_config flags
get_files(State) ->
    WatchSSL    = maps:get(watch_ssl, State, true),
    WatchConfig = maps:get(watch_config, State, false),

    CertFiles =
        case WatchSSL of
            true  -> get_certfiles();
            false -> []
        end,

    ConfigFiles =
        case WatchConfig of
            true  -> get_configfiles();
            false -> []
        end,

    lists:usort(CertFiles ++ ConfigFiles).

get_certfiles() ->
    case ejabberd_option:certfiles() of
        undefined ->
            [];
        List when is_list(List) ->
            [normalize(F) || F <- List]
    end.

get_configfiles() ->
    %% Main ejabberd.yml
    Main =
        case catch ejabberd_config:path() of
            {'EXIT', _} ->
                [];
            Path ->
                [normalize(Path)]
        end,

    %% Configuration files of external modules
    ModuleConfigs =
        case catch ext_mod:modules_configs() of
            {'EXIT', _} -> [];
            L when is_list(L) -> [normalize(F) || F <- L];
            Other -> [normalize(Other)]
        end,

    %% Additional config files from include_config_file:
    Includes =
        case catch ejabberd_option:include_config_file() of
            {'EXIT', _} ->
                [];
            undefined ->
                [];
            Files when is_list(Files) ->
                [normalize(F) || F <- Files];
            F ->
                [normalize(F)]
        end,

    lists:usort(Main ++ ModuleConfigs ++ Includes).

%% Normalize file paths to binaries for consistent internal representation
normalize(Bin) when is_binary(Bin) ->
    Bin;
normalize(Str) when is_list(Str) ->
    list_to_binary(Str).

%% Return true if any file has mtime > LastReload (POSIX seconds)
any_file_changed(Files, LastReload) ->
    lists:any(
      fun(File) ->
          case file_mtime_posix(File) of
              {ok, MTime} when MTime > LastReload ->
                  true;
              _ ->
                  false
          end
      end,
      Files).

%% Get file mtime as POSIX seconds using file:read_file_info/2
file_mtime_posix(FileBin) ->
    Filename = binary_to_list(FileBin),
    case file:read_file_info(Filename, [{time, posix}]) of
        {ok, #file_info{mtime = MTime}} ->
            {ok, MTime};
        Error ->
            Error
    end.