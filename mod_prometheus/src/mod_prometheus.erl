-module(mod_prometheus).
-author('pouriya.jahanbakhsh@gmail.com').
-author('stefan@strigler.de').
-behaviour(gen_mod).
-behaviour(gen_server).

%% gen_mod callbacks
-export([start/2, stop/1, reload/3, mod_doc/0,
         mod_opt_type/1, mod_options/1,
         depends/2]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

%% internal API
-export([process_histogram/5, process_counter/5]).

-export([process/2]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_http.hrl").

-define(INTERVAL, 1000).

start(Host, Opts) ->
  case lists:member({subscribe, 5}, ejabberd_hooks:module_info(exports)) of
    true ->
      application:set_env(prometheus, collectors, []),
      ejabberd:start_app(prometheus),
      update_mnesia(get_opt(mnesia, Opts)),
      update_vm(get_opt(vm, Opts)),
      handle_hooks(get_opt(hooks, Opts), Host, subscribe, []),
      gen_mod:start_child(?MODULE, Host, Opts);
    _ ->
      Error = "Hook subscriber is not supported. Please Upgrade Ejabberd.",
      ?ERROR_MSG(Error, []),
      {error, Error}
  end.

init([Host|_]) ->
  process_flag(trap_exit, true),
  Opts = gen_mod:get_module_opts(Host, ?MODULE),
  TRef = start_timer(),
  {ok, [Host, Opts, TRef]}.

terminate(_Reason, _State) ->
  ok.

handle_call(stop, _From, [_Host, _Opts, TRef] = State) ->
  misc:cancel_timer(TRef),
  {stop, normal, ok, State};
handle_call(Request, From, State) ->
  ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
  {noreply, State}.

handle_cast({reload, NewOpts}, [Host, _OldOpts, TRef]) ->
  ?DEBUG("Reloading opts: ~p", [NewOpts]),
  {noreply, [Host, NewOpts, TRef]};
handle_cast(Msg, State) ->
  ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
  {noreply, State}.

handle_info({timeout, _TRef, ping}, [Host, Opts, OldTRef]) ->
  ?DEBUG("Running commands for ~p", [Host]),
  lists:foreach(
    fun(#{type := gauge, hook := Hook, command := Cmd} = Opt) ->
        OptArgs = maps:get(args, Opt, []),
        CmdArgs = replace_host(OptArgs, Host, []),
        Res = ejabberdctl(Cmd, CmdArgs, maps:get(result_type, Opt, raw)),
        ?DEBUG("Got result for command ~p with args ~p on ~s: ~p", [Cmd, CmdArgs, Host, Res]),
        LabelNames = maps:get(labels, Opt, []),
        Labels = replace_host(LabelNames, Host, []),
        prometheus_gauge:set(Hook, Labels, Res);
       (_) -> noop
    end,
    maps:get(hooks, Opts)
   ),
  misc:cancel_timer(OldTRef),
  TRef = start_timer(),
  {noreply, [Host, Opts, TRef]};
handle_info(Info, State) ->
  ?WARNING_MSG("Unexpected info: ~p", [Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

start_timer() ->
  erlang:start_timer(?INTERVAL, self(), ping).

replace_host([], _Host, Res) ->
  lists:reverse(Res);
replace_host([host | Args], Host, Res) ->
  replace_host(Args, Host, [Host | Res]);
replace_host([Arg | Args], Host, Res) ->
  replace_host(Args, Host, [Arg | Res]).

ejabberdctl(Cmd, Args, raw) ->
  ejabberd_commands:execute_command2(Cmd, Args, #{caller_module => ejabberd_ctl}, 10000);
ejabberdctl(Cmd, Args, list) ->
  length(ejabberdctl(Cmd, Args, raw)).

stop(Host) ->
  handle_hooks(get_opt(hooks, Host), Host, unsubscribe, []),
  gen_mod:stop_child(?MODULE, Host).

reload(Host, NewOpts, OldOpts) ->
  prometheus_registry:clear(),
  handle_hooks(get_opt(hooks, OldOpts), Host, unsubscribe, []),
  handle_hooks(get_opt(hooks, NewOpts), Host, subscribe, []),
  update_mnesia(get_opt(mnesia, NewOpts)),
  update_vm(get_opt(vm, NewOpts)),
  Proc = gen_mod:get_module_proc(Host, ?MODULE),
  gen_server:cast(Proc, {reload, NewOpts}),
  ok.

depends(_Host, _Opts) ->
  [].

mod_opt_type(vm) ->
  econf:options(
    #{
      distribution => econf:bool(),
      memory => econf:bool(),
      microstate_accounting => econf:bool(),
      statistics => econf:bool(),
      system_info => econf:bool()
    },
    [{return, map}]
  );
mod_opt_type(mnesia) ->
  econf:bool();
mod_opt_type(hooks) ->
  econf:list(
    econf:options(
      #{
        hook => econf:atom(),
        type => econf:enum([histogram, counter, gauge]),
        buckets => econf:list(econf:int(0, 150000)),
        labels => econf:list(econf:enum([host, stanza, module])),
        help => econf:string(),
        command => econf:atom(),
        args => econf:list(econf:either(host, econf:binary())),
        result_type => econf:enum([raw, list]),
        collect => econf:either(
          all,
          econf:list(
            econf:options(
              #{
                module => econf:atom(),
                function => econf:atom(),
                type => econf:enum([histogram, counter]),
                buckets => econf:list(econf:int(0, 150000)),
                labels => econf:list(econf:enum([host, stanza])),
                help => econf:string()
              },
              [{required, [module, function]}, {return, map}]
            )
          )
        )
      },
      [{required, [hook]}, {return, map}, unique]
    ),
    [unique]
  ).

mod_options(_Host) ->
  [{vm, #{}}, {mnesia, false}, {hooks, []}].

mod_doc() ->
  #{desc => [<<"TODO">>], opts => []}.

get_opt(Key, #{}=Opts) ->
  gen_mod:get_opt(Key, Opts);
get_opt(Key, Host) ->
  gen_mod:get_module_opt(Host, mod_prometheus, Key).

process(_Path, #request{}=_Request) ->
  {200, [{<<"Content-Type">>, <<"text/plain; version=0.0.4; charset=utf-8; escaping=values">>}], prometheus_text_format:format()}.

update_mnesia(Enable) ->
  application:set_env(
    prometheus,
    mnesia_collector_metrics,
    if
      Enable ->
        prometheus_registry:register_collector(prometheus_mnesia_collector),
        ?INFO_MSG("Enabled Mnesia Prometheus metrics", []),
        all;
      true ->
        ?INFO_MSG("Disabled Mnesia Prometheus metrics", []),
        []
    end),
  ok.

update_vm(Opts) ->
  lists:foreach(
    fun({Name, EnvName, Mod}) ->
      application:set_env(
        prometheus,
        EnvName,
        case maps:get(Name, Opts, false) of
          true ->
            prometheus_registry:register_collector(Mod),
            ?INFO_MSG("Enabled Erlang VM ~p Prometheus metrics", [Name]),
            all;
          _ ->
            ?INFO_MSG("Disabled Erlang VM ~p Prometheus metrics", [Name]),
            []
        end
      )
    end,
    [
      {distribution, vm_dist_collector_metrics, prometheus_vm_dist_collector},
      {memory, vm_memory_collector_metrics, prometheus_vm_memory_collector},
      {microstate_accounting, prometheus_vm_dist_collector, prometheus_vm_msacc_collector},
      {statistics, vm_statistics_collector_metrics, prometheus_vm_statistics_collector},
      {system_info, vm_system_info_collector_metrics, prometheus_vm_system_info_collector}
    ]
  ).

%% When the runner starts running this hook
%% Hook =:= Hook:
process_histogram(#{hook := Hook}=InitArg, before, _Host, Hook, _) ->
  InitArg#{hook_duration_start_time => erlang:system_time(millisecond)};
%% When the runner stops running this hook
%% Hook =:= Hook:
process_histogram(
    #{name := Name, hook := Hook, labels := LabelNames, hook_duration_start_time := Time}=State,
    'after',
    Host,
    Hook,
    Args
) ->
  Duration = erlang:system_time(millisecond) - Time,
  Labels = replace_labels(LabelNames, Args, Host, "total"),
  prometheus_histogram:observe(Name, Labels, Duration),
  maps:remove(hook_duration_start_time, State);
%% When runner start running a callback and `labels` section contains `module`:
%% Hook =:= Hook:
process_histogram(
    #{hook := Hook, labels := LabelNames}=State,
    before_callback,
    _Host,
    Hook,
    {_Mod, _Func, _Seq, _Args}
) ->
  case lists:member(module, LabelNames) of
    true ->
      State#{module_duration_start_time => erlang:system_time(millisecond)};
    _ ->
      State
  end;
%% When runner is done running a callback and `labels` section contains `module`:
%% Hook =:= Hook:
process_histogram(
    #{hook := Hook, name := Name, module_duration_start_time := Time}=State,
    after_callback,
    Host,
    Hook,
    {Mod, _Func, _Seq, Args}
) ->
  Duration = erlang:system_time(millisecond) - Time,
  LabelNames = maps:get(labels, State, []),
  Labels = replace_labels(LabelNames, Args, Host, Mod),
  prometheus_histogram:observe(Name, Labels, Duration),
  maps:remove(module_duration_start_time, State);
%% When runner runs a callback and we are going to collect this callback info:
%% {Mod, Func} =:= {Mod, Func}:
process_histogram(#{callback := {Mod, Func}}=State, before_callback, _Host, _Hook, {Mod, Func, _Seq, _Args}) ->
  State#{callback_duration_start_time => erlang:system_time(millisecond)};
%% When runner done running a callback and we are going to collect this callback info:
%% {Mod, Func} =:= {Mod, Func}:
process_histogram(
    #{callback := {Mod, Func}, name := Name, callback_duration_start_time := Time}=State,
    after_callback,
    Host,
    _Hook,
    {Mod, Func, _Seq, Args}
) ->
  Duration = erlang:system_time(millisecond) - Time,
  LabelNames = maps:get(labels, State, []),
  Labels = replace_labels(LabelNames, Args, Host, Mod),
  prometheus_histogram:observe(Name, Labels, Duration),
  maps:remove(callback_duration_start_time, State);
process_histogram(#{}=State, _Event, _Host, _Hook, _) ->
  State.

%% When the runner starts running this hook
%% Hook =:= Hook:
process_counter(#{hook := Hook}=InitArg, before, _Host, Hook, _) ->
  InitArg;
%% When the runner stops running this hook
%% Hook =:= Hook:
process_counter(
    #{name := Name, hook := Hook, labels := LabelNames}=State,
    'after',
    Host,
    Hook,
    Args
) ->
  Labels = replace_labels(LabelNames, Args, Host, "total"),
  prometheus_counter:inc(Name, Labels),
  State;
%% When runner start running a callback and `labels` section contains `module`:
%% Hook =:= Hook:
process_counter(
    #{hook := Hook}=State,
    before_callback,
    _Host,
    Hook,
    {_Mod, _Func, _Seq, _Args}
) ->
  State;
%% When runner is done running a callback and `labels` section contains `module`:
%% Hook =:= Hook:
process_counter(
    #{hook := Hook, name := Name}=State,
    after_callback,
    Host,
    Hook,
    {Mod, _Func, _Seq, Args}
) ->
  LabelNames = maps:get(labels, State, []),
  case lists:member(module, LabelNames) of
    true ->
      Labels = replace_labels(LabelNames, Args, Host, Mod),
      prometheus_counter:inc(Name, Labels);
    _ ->
      ok
  end,
  State;
%% When runner runs a callback and we are going to collect this callback info:
%% {Mod, Func} =:= {Mod, Func}:
process_counter(#{callback := {Mod, Func}}=State, before_callback, _Host, _Hook, {Mod, Func, _Seq, _Args}) ->
  State;
%% When runner done running a callback and we are going to collect this callback info:
%% {Mod, Func} =:= {Mod, Func}:
process_counter(
    #{callback := {Mod, Func}, name := Name}=State,
    after_callback,
    Host,
    _Hook,
    {Mod, Func, _Seq, Args}
) ->
  LabelNames = maps:get(labels, State, []),
  Labels = replace_labels(LabelNames, Args, Host, Mod),
  prometheus_counter:inc(Name, Labels),
  State;
process_counter(#{}=State, _Event, _Host, _Hook, _) ->
  State.

handle_hooks([HookOpts | Hooks], Host, Action, Metrics) ->
  handle_hooks(Hooks, Host, Action, Metrics ++ handle_hook(HookOpts));
handle_hooks([], Host, Action, [{Type, Name, MName, Opts, InitArg} | Metrics]) ->
  case lists:any(
    fun({_, _, OtherMName, _, _}) -> MName == OtherMName end,
    Metrics
  ) of
    true ->
      ?ERROR_MSG("Dropped Prometheus duplicate metric name ~tp for hook ~tp", [MName, Name]);
    _ ->
      case Type of
        histogram ->
          handle_histogram(Name, MName, Opts, Host, Action, InitArg);
        counter ->
          handle_counter(Name, MName, Opts, Host, Action, InitArg);
        gauge ->
          handle_gauge(Name, MName, Opts, Host, Action, InitArg)
      end
  end,
  handle_hooks([], Host, Action, Metrics);
handle_hooks([], _Host, _Action, []) ->
  ok.

handle_hook(#{hook := Name}=Opts) ->
  Collect = maps:get(collect, Opts, all),
  Type = maps:get(type, Opts, histogram),
  case Type of
    histogram ->
      case Collect of
        all ->
          [{Type, Name, duration_histogram_name(Name), Opts, #{hook => Name}}];
        Callbacks ->
          Opts2 = maps:remove(collect, Opts),
          Labels = maps:get(labels, Opts, []),
          [
            {
              Type,
              Name,
              duration_histogram_name(Name, Mod, Func),
              (maps:merge(Opts2, Callback))#{
                %% Merge labels:
                labels => sets:to_list(sets:from_list(Labels ++ maps:get(labels, Callback, [])))
              },
              #{callback => {Mod, Func}}
            } || #{module := Mod, function := Func}=Callback <- Callbacks
          ]
      end;
    counter ->
      case Collect of
        all ->
          [{Type, Name, counter_name(Name), Opts, #{hook => Name}}];
        Callbacks ->
          Opts2 = maps:remove(collect, Opts),
          Labels = maps:get(labels, Opts, []),
          [
            {
              Type,
              Name,
              counter_name(Name, Mod, Func),
              (maps:merge(Opts2, Callback))#{
                %% Merge labels:
                labels => sets:to_list(sets:from_list(Labels ++ maps:get(labels, Callback, [])))
              },
              #{callback => {Mod, Func}}
            }
            || #{module := Mod, function := Func}=Callback <- Callbacks
          ]
      end;
    gauge ->
      [{Type, Name, Name, Opts, #{hook => Name}}]
  end.

handle_histogram(Name, HName, HistogramOpts, Host, Action, State) ->
  LabelNames = maps:get(labels, HistogramOpts, []),
  InitArg = State#{name => HName, labels => LabelNames},
  case Action of
    subscribe ->
      prometheus_histogram:declare(
        [
          {name, HName},
          {buckets, maps:get(buckets, HistogramOpts, [1, 10, 100, 500, 750, 1000, 3000, 5000])},
          {help, maps:get(help, HistogramOpts, "No help")},
          {labels, LabelNames}
        ]
      ),
      ?INFO_MSG("Created new Prometheus histogram for ~p with labels ~p", [HName, LabelNames]),
      ejabberd_hooks:subscribe(Name, Host, ?MODULE, process_histogram, InitArg);
    _ ->
      try prometheus_histogram:deregister(HName) of
        _ ->
          ?INFO_MSG("Removed Prometheus histogram for ~p with labels ~p", [HName, LabelNames])
      catch _:{unknown_metric, _, _} ->
        ok
      end,
      ejabberd_hooks:unsubscribe(Name, Host, ?MODULE, process_histogram, InitArg)
  end.

handle_counter(Name, HName, CounterOpts, Host, Action, State) ->
  LabelNames = maps:get(labels, CounterOpts, []),
  InitArg = State#{name => HName, labels => LabelNames},
  case Action of
    subscribe ->
      prometheus_counter:declare(
        [{name, HName}, {help, maps:get(help, CounterOpts, "No help")}, {labels, LabelNames}]
      ),
      ?INFO_MSG("Created new Prometheus counter for ~p with labels ~p", [HName, LabelNames]),
      ejabberd_hooks:subscribe(Name, Host, ?MODULE, process_counter, InitArg);
    _ ->
      try prometheus_counter:deregister(HName) of
        _ ->
          ?INFO_MSG("Removed Prometheus counter for ~p with labels ~p", [HName, LabelNames])
      catch _:{unknown_metric, _, _} ->
        ok
      end,
      ejabberd_hooks:unsubscribe(Name, Host, ?MODULE, process_counter, InitArg)
  end.

handle_gauge(_Name, GName, GaugeOpts, _Host, Action, _State) ->
  LabelNames = maps:get(labels, GaugeOpts, []),
  case Action of
    subscribe ->
      prometheus_gauge:declare(
        [{name, GName}, {help, maps:get(help, GaugeOpts, "No help")}, {labels, LabelNames}]
       ),
      ?INFO_MSG("Created new Prometheus gauge for ~p with labels ~p", [GName, LabelNames]);
    _ ->
      try prometheus_gauge:deregister(GName) of
        _ ->
          ?INFO_MSG("Removed Prometheus gauge for ~p with labels ~p", [GName, LabelNames])
      catch _:{unknown_metric, _, _} ->
        ok
      end
  end.

duration_histogram_name(Hook) ->
  list_to_atom(atom_to_list(Hook) ++ "_duration_milliseconds").

duration_histogram_name(Hook, Mod, Func) when Hook =:= Func ->
  list_to_atom(atom_to_list(Hook) ++ "_" ++ atom_to_list(Mod) ++ "_duration_milliseconds");
duration_histogram_name(Hook, Mod, Func) ->
  list_to_atom(atom_to_list(Hook) ++ "_" ++ atom_to_list(Mod) ++ atom_to_list(Func) ++ "_duration_milliseconds").

counter_name(Hook) ->
  list_to_atom(atom_to_list(Hook) ++ "_total").

counter_name(Hook, Mod, Func) when Hook =:= Func ->
  list_to_atom(atom_to_list(Hook) ++ "_" ++ atom_to_list(Mod) ++ "_total");
counter_name(Hook, Mod, Func) ->
  list_to_atom(atom_to_list(Hook) ++ "_" ++ atom_to_list(Mod) ++ atom_to_list(Func) ++ "_total").

maybe_remove_module_prefix(Mod) when is_atom(Mod) ->
  maybe_remove_module_prefix(atom_to_list(Mod));
maybe_remove_module_prefix("mod_" ++ Name) ->
  Name;
maybe_remove_module_prefix(Name) ->
  Name.

replace_labels([host | LabelNames], HookArgs, Host, Mod) when Host /= undefined ->
  [Host | replace_labels(LabelNames, HookArgs, Host, Mod)];
replace_labels([stanza | LabelNames], HookArgs, Host, Mod) when HookArgs /= undefined ->
  [find_stanza_type(HookArgs) | replace_labels(LabelNames, HookArgs, Host, Mod)];
replace_labels([module | LabelNames], HookArgs, Host, Mod) when Mod /= undefined ->
  [maybe_remove_module_prefix(Mod) | replace_labels(LabelNames, HookArgs, Host, Mod)];
replace_labels([_Unwanted | LabelNames], HookArgs, Host, Mod) ->
  replace_labels(LabelNames, HookArgs, Host, Mod);
replace_labels([], _, _, _) ->
  [].

find_stanza_type({Arg, _}) when ?is_stanza(Arg) -> %% c2s
  element(1, Arg);
find_stanza_type([{Arg, _} | _]) when ?is_stanza(Arg) -> %% c2s
  element(1, Arg);
find_stanza_type([Arg | _]) when ?is_stanza(Arg) ->
  element(1, Arg);
find_stanza_type(Tup) when is_tuple(Tup) ->
  find_stanza_type(erlang:tuple_to_list(Tup));
find_stanza_type([Arg | Args]) when is_tuple(Arg) ->
  find_stanza_type(erlang:tuple_to_list(Arg) ++ Args);
find_stanza_type([_ | Args]) ->
  find_stanza_type(Args);
find_stanza_type([]) ->
  unknown.
