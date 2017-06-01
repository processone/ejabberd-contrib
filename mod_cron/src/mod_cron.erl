%%%----------------------------------------------------------------------
%%% File    : mod_cron.erl
%%% Author  : Badlop
%%% Purpose : Execute scheduled tasks
%%% Created : 12 July 2007
%%% Id      : $Id: mod_cron.erl 1034 2009-11-17 21:44:17Z badlop $
%%%----------------------------------------------------------------------

-module(mod_cron).
-author('badlop@ono.com').

-behaviour(gen_mod).

-export([
	 cron_list/1, cron_del/1,
	 run_task/3,
	 web_menu_host/3, web_page_host/3,
	 start/2,
         apply_interval/3,
         apply_interval1/3,
	 stop/1]).

-include("ejabberd_commands.hrl").
-include("ejabberd.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-record(task, {taskid, timerref, host, task}).

%% ---------------------
%% gen_mod
%% ---------------------

start(Host, Opts) ->
    ejabberd_commands:register_commands(commands()),
    ejabberd_hooks:add(webadmin_menu_host, Host, ?MODULE, web_menu_host, 50),
    ejabberd_hooks:add(webadmin_page_host, Host, ?MODULE, web_page_host, 50),
    Tasks = gen_mod:get_opt(tasks, Opts, []),
    catch ets:new(cron_tasks, [ordered_set, named_table, public, {keypos, 2}]),
    [add_task(Host, Task) || Task <- Tasks],
    ok.

stop(Host) ->
    ejabberd_commands:unregister_commands(commands()),
    ejabberd_hooks:delete(webadmin_menu_host, Host, ?MODULE, web_menu_host, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host, ?MODULE, web_page_host, 50),
    %% Delete tasks of this host
    [delete_task(Task) || Task <- get_tasks(Host)],
    ok.


%% ---------------------
%% Task management
%% ---------------------

time_to_ms(IntervalUnit, IntervalNum) ->
    case IntervalUnit of
        seconds -> timer:seconds(IntervalNum);
        minutes -> timer:minutes(IntervalNum);
        hours -> timer:hours(IntervalNum);
        days -> timer:hours(IntervalNum)*24
    end.

time_until_event(IntervalMS) ->
    NowMS = p1_time_compat:system_time(micro_seconds),
    MSSinceLastEvent = (NowMS rem IntervalMS),
    (IntervalMS - MSSinceLastEvent).

begin_interval_timer(TaskId, TimeUnit, TimeNum, StartParams) ->
    IntervalMS = time_to_ms(TimeUnit, TimeNum),
    MSToGo = time_until_event(IntervalMS),
    {ok, TimerRef} = timer:apply_after(MSToGo, ?MODULE, apply_interval,
                                       [TaskId, IntervalMS, StartParams]),
    TimerRef.

begin_fixed_timer(TaskId, TimeUnit, TimeNum, StartParams) ->
    %% A fixed second timer happens minutely, minute timer happens hourly, a fixed hour timer happens daily.
    IntervalMS = case TimeUnit of
                     seconds -> timer:minutes(1);
                     minutes -> timer:hours(1);
                     hours -> timer:hours(1) * 24;
                     _ -> undefined
                 end,

    FixedTimeMS = time_to_ms(TimeUnit, TimeNum),

    %% Calculate time until the next IntervalUnit, then add FixedTimeMS
    %% e.g. now is 00:00:32 wait until the next minute (28s), then keep waiting
    %% 5 more seconds to get 00:01:05 (= wait 33s).
    %% We then fire the event at 00:01:05 and wait a minute to fire again
    %% at 00:02:05 etc.
    MSToGo1 = time_until_event(IntervalMS) + FixedTimeMS,

    %% If we were, for example, at 1:03PM and the event is hourly at
    %% 1:05PM then we dont want to wait 57+5 minutes, we want to wait
    %% 2 minutes.
    MSToGo2 = if MSToGo1 > IntervalMS ->
                      MSToGo1 - IntervalMS;
                 true ->
                      MSToGo1
              end,

    ?DEBUG("MS To Go Fixed: ~p ~p", [MSToGo1, MSToGo2]),
    {ok, TimerRef} = timer:apply_after(MSToGo2, ?MODULE, apply_interval, [TaskId, IntervalMS, StartParams]),
    TimerRef.

apply_interval(TaskId, IntervalMS, StartParams) ->
    %% apply_after doesnt belong to a pid (which is needed for apply_after to stay alive), so make one
    spawn(?MODULE, apply_interval1, [TaskId, IntervalMS, StartParams]).

apply_interval1(TaskId, IntervalMS, [M, F, A]=StartParams) ->
    % we've already waited for the interval to expire once to get here,
    % and apply_interval doesn't apply first, so run the task once then start the timer
    run_task(M, F, A),
    {ok, TimerRef} = timer:apply_interval(IntervalMS, ?MODULE, run_task, StartParams),
    update_timer_ref(TaskId, TimerRef),

    %% Wait forever so the timer process stays alive
    receive
        _ ->
            ok
    end.

update_timer_ref(TaskId, NewTimerRef) ->
    [Task] = ets:lookup(cron_tasks, TaskId),
    NewTask = Task#task{timerref=NewTimerRef},
    ets:insert(cron_tasks, NewTask).

%% Method to add new task
add_task(Host, Task) ->
    [TimeNum, TimeUnit, Mod, Fun, Args, InTimerType] =
	[proplists:get_value(Key, Task) || Key <- [time, units, module, function, arguments, timer_type]],
    TimerType = case InTimerType of
                    <<"fixed">> ->
                        fixed;
                    fixed ->
                        fixed;
                    _ ->
                        interval
                end,

    %% Get new task identifier
    TaskId = get_new_taskid(),

    TimerRef = case TimerType of
                   interval ->
                       begin_interval_timer(TaskId, TimeUnit, TimeNum, [Mod, Fun, Args]);
                   fixed ->
                       begin_fixed_timer(TaskId, TimeUnit, TimeNum, [Mod, Fun, Args])
               end,

    %% Store TRef
    Taskr = #task{
      taskid = TaskId,
      timerref = TimerRef,
      host = Host,
      task = Task
     },

    ets:insert(cron_tasks, Taskr).

get_new_taskid() ->
    case ets:last(cron_tasks) of
	'$end_of_table' -> 0;
	Id -> Id + 1
    end.

%% Method to run existing task
run_task(Mod, Fun, Args) ->
    case catch apply(Mod, Fun, Args) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("Error in scheduled task ~p:~p~p:~n~p", [Mod, Fun, Args, Reason]);
	{error, Reason} ->
	    ?ERROR_MSG("Error in scheduled task ~p:~p~p:~n~p", [Mod, Fun, Args, Reason]);
	ok ->
	    ?INFO_MSG("Scheduled task ~p:~p~p finished ok", [Mod, Fun, Args]);
	Res ->
	    ?INFO_MSG("Scheduled task ~p:~p~p returned:~n~p", [Mod, Fun, Args, Res])
    end.

%% Method to delete task, given a taskid
delete_taskid(TaskId) ->
    [Task] = ets:lookup(cron_tasks, TaskId),
    delete_task(Task).

%% Method to delete task, given the whole task
delete_task(Task) ->
    timer:cancel(Task#task.timerref),
    ets:delete(cron_tasks, Task#task.taskid).

%% Method to know existing tasks on a given host
get_tasks(Host) ->
    ets:select(cron_tasks, [{#task{host = Host, _ = '_'}, [], ['$_']}]).

%% Method to know taskids of existing tasks on a given host
%%get_tasks_ids(Host) ->
%%	L = ets:match(cron_tasks, #task{host = Host, taskid = '$1', _ = '_'}),
%%	[Id || [Id] <- L].


%% ---------------------
%% ejabberd commands
%% ---------------------

commands() ->
    [
     #ejabberd_commands{name = cron_list, tags = [cron],
		       desc = "List tasks scheduled in a host",
		       module = ?MODULE, function = cron_list,
		       args = [{host, binary}],
			   result = {tasks, {list, {task, {tuple, [{id, integer}, {task, string}]}}}}},
     #ejabberd_commands{name = cron_del, tags = [cron],
		       desc = "Delete this task from the schedule",
		       module = ?MODULE, function = cron_del,
		       args = [{taskid, integer}],
			   result = {res, rescode}}
    ].

cron_list(Host) ->
    Tasks = get_tasks(Host),
    [{T#task.taskid, io_lib:format("~p", [T#task.task])} || T <- Tasks].

cron_del(TaskId) ->
    delete_taskid(TaskId),
	ok.


%% ---------------------
%% Web Admin
%% ---------------------

web_menu_host(Acc, _Host, Lang) ->
    [{<<"cron">>, ?T(<<"Cron Tasks">>)} | Acc].

web_page_host(_, Host,
	      #request{path = [<<"cron">>],
		       lang = Lang} = _Request) ->
    Tasks = get_tasks(Host),
    Tasks_table = make_tasks_table(Tasks, Lang),
    Res = [?XC(<<"h1">>, <<"Cron Tasks">>)] ++ Tasks_table,
    {stop, Res};
web_page_host(Acc, _, _) -> Acc.

make_tasks_table(Tasks, Lang) ->
    TList = lists:map(
	      fun(T) ->
		      {Time_num, Time_unit, Mod, Fun, Args} = T#task.task,
		      ?XE(<<"tr">>,
			  [?XC(<<"td">>, list_to_binary(integer_to_list(Time_num) ++" " ++ atom_to_list(Time_unit))),
			   ?XC(<<"td">>, list_to_binary(atom_to_list(Mod))),
			   ?XC(<<"td">>, list_to_binary(atom_to_list(Fun))),
			   ?XC(<<"td">>, list_to_binary(io_lib:format("~p", [Args])))])
	      end, Tasks),
    [?XE(<<"table">>,
	 [?XE(<<"thead">>,
	      [?XE(<<"tr">>,
		   [?XCT(<<"td">>, <<"Periodicity">>),
		    ?XCT(<<"td">>, <<"Module">>),
		    ?XCT(<<"td">>, <<"Function">>),
		    ?XCT(<<"td">>, <<"Arguments">>)])]),
	  ?XE(<<"tbody">>, TList)])].
