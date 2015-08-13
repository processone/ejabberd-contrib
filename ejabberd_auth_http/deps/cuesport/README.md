Summary
=======

Cuesport is a simple pool of workers, meant mainly to be used with things like DB connections.
Use when poolboy's elaborate checkin/checkout seems to be overkill for your task.

Usage
-----

Suggested usage is to create your own wrapper module which wraps cuesport and provides
wrapper functions to hide pool's usage. Something in the lines of:

```erlang
-module(myproduct_redis).

-export([start_link/0, q/1, q/2]).

-define(POOL_NAME, myproduct_redis_pool).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    PoolSize = get_config(redis_pool_size),
    EredisOpts = get_config(redis_worker_config),
    ChildMods = [eredis, eredis_client, eredis_parser],
    ChildMFA = {eredis, start_link, EredisOpts},
    cuesport:start_link(?POOL_NAME, PoolSize, ChildMods, ChildMFA).

q(Query) ->
    eredis:q(cuesport:get_worker(?POOL_NAME), Query).

q(Query, Opts) ->
    eredis:q(cuesport:get_worker(?POOL_NAME), Query, Opts).
```

Then add `myproduct_redis` to your supervision tree.
