-module(write).
-export([start/2, start/3]).

dump_file(F, Data, 0) ->
    ok;
dump_file(F, Data, N) ->
    bfile:fwrite(F, Data),
    dump_file(F, Data, N - 1).

start(File, Data, N) ->
    bfile:load_driver(),
    {ok, F} = bfile:fopen(File, "w"),
    dump_file(F, Data, N),
    bfile:fclose(F).
start(File, N) ->
    Data = list_to_binary(lists:duplicate(1000000, 10)),
    start(File, Data, N).
