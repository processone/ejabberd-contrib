-module(writeold).
-export([start/2, start/3]).

dump_file(F, Data, 0) ->
    ok;
dump_file(F, Data, N) ->
    file:write(F, Data),
    dump_file(F, Data, N - 1).

start(File, Data, N) ->
    {ok, F} = file:open(File, [raw, binary, write]),
    dump_file(F, Data, N),
    file:close(F).
start(File, N) ->
    Data = list_to_binary(lists:duplicate(1000000, 10)),
    start(File, Data, N).
