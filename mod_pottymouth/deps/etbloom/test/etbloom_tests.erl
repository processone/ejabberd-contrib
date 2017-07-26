%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
-module(etbloom_tests).
-author("volodymyr.kyrychenko@strikead.com").
-include_lib("eunit/include/eunit.hrl").

bloom_test() ->
    Values = [{xxx, binary_to_atom(base64:encode(crypto:strong_rand_bytes(10)), utf8)} || _ <- lists:seq(1, 5000)],
    Bloom = etbloom:bloom(Values),
    ?assert(lists:all(fun(X) -> etbloom:member(X, Bloom) end, Values)),
    ?assertNot(etbloom:member(wtf, Bloom)).
