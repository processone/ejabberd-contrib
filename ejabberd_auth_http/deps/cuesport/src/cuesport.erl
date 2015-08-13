%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @doc Utility for using a pool of workers easily
%%% Obvious pun is obvious.
%%% @end
%%%===================================================================

-module(cuesport).
-behaviour(supervisor).

%% API
-export([start_link/4,
         start_link/5,
         get_worker/1]).

%% Supervisor callbacks
-export([init/1]).

%% Internal exports
-export([start_worker/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link(atom(), integer(), [atom()], {atom(), atom(), [term()]}) ->
        {ok, pid()} | ignore | {error, term()}.
start_link(PoolName, PoolSize, ChildMods, ChildMFA) ->
    start_link(PoolName, PoolSize, 2*PoolSize, ChildMods, ChildMFA).

-spec start_link(atom(), integer(), integer(), [atom()], {atom(), atom(), [term()]}) ->
        {ok, pid()} | ignore | {error, term()}.
start_link(PoolName, PoolSize, MaxRestarts, ChildMods, ChildMFA) ->
    Args = [PoolName, PoolSize, MaxRestarts, ChildMods, ChildMFA],
    SupName = list_to_atom("cuesport_" ++ atom_to_list(PoolName) ++ "_sup"),
    supervisor:start_link({local, SupName}, ?MODULE, Args).

-spec get_worker(atom()) -> pid().
get_worker(PoolName) ->
    [{pool_size, PoolSize}] = ets:lookup(PoolName, pool_size),
    N = ets:update_counter(PoolName, seq, {2, 1, PoolSize, 1}),
    [{N, Worker}] = ets:lookup(PoolName, N),
    Worker.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([PoolName, PoolSize, MaxRestarts, ChildMods, ChildMFA]) ->
    PoolTable = ets:new(PoolName, [named_table, public]),
    ets:insert(PoolTable, {pool_size, PoolSize}),
    ets:insert(PoolTable, {seq, 0}),

    MFA = fun(Id) ->
            {?MODULE, start_worker, [Id, PoolTable, ChildMFA]}
    end,
    Children = [{N, MFA(N), transient, 2000, worker, ChildMods}
        || N <- lists:seq(1, PoolSize)],

    {ok, {{one_for_one, MaxRestarts, PoolSize}, Children}}.

%%%===================================================================
%%% non-API exports
%%%===================================================================

-spec start_worker(integer(), atom(), {module(), atom(), [term()]}) -> {ok, pid()}.
start_worker(Id, PoolTable, {M, F, A}) ->
    {ok, Pid} = apply(M, F, A),
    ets:insert(PoolTable, {Id, Pid}),
    {ok, Pid}.
