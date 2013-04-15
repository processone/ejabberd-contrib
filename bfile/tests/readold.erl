-module(readold).
-export([start/1, start/2]).

scan_file(F, Readsize, Total) ->
   Rd = file:read(F, Readsize),
   case Rd of
       {ok, Bin} -> scan_file(F, Readsize, size(Bin)+Total);
       eof -> Total
   end.
scan_file(F, Readsize) -> scan_file(F, Readsize, 0).

start(File, Readsize) ->
   {ok, F} = file:open(File, [raw, binary, read]),
   T = scan_file(F, Readsize),
   io:format("read ~p bytes~n", [T]),
   file:close(F).
start(File) ->
   start(File, 512*1024).
