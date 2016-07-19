-module(bloom_gen_server).

-behaviour(gen_server).

-include("logger.hrl").

-import(etbloom, [bloom/1, member/2]).
-export([member/1]).

%% gen_server callbacks
-export([start/1, stop/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

serverName(Lang) ->
  list_to_atom(lists:flatten([atom_to_list(?MODULE), "_", atom_to_list(Lang)])).

member({Lang, Word} = _MessageToken) ->
  gen_server:call(serverName(Lang), {member, Word}).

start({Lang, BlacklistFile} = _Opts) ->
  gen_server:start_link({local, serverName(Lang)}, ?MODULE, [BlacklistFile], []).

stop() ->
    ok.

init([BlacklistFile]) ->
  ?INFO_MSG("Building bloom ~p~n", [BlacklistFile]),
  Bloom = etbloom:sbf(10000000),
  {ok, loadWordList(Bloom, BlacklistFile)}.

handle_call({member, Word}, _From, Bloom) ->
  Reply = etbloom:member(Word, Bloom),
  {reply, Reply, Bloom}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

loadWordList(Bloom, BlacklistFile) ->
  BlacklistExists = filelib:is_file(BlacklistFile),
  if
    BlacklistExists ->
      {ok, S} = file:open(BlacklistFile, read),
      loadWordList(io:get_line(S, ''), Bloom, S);
    true ->
      ?ERROR_MSG("Blacklist file not found: ~p~n", [BlacklistFile]),
      loadWordList(eof, Bloom, BlacklistFile)
  end.

loadWordList(eof, Bloom, _S) ->
  Bloom;
loadWordList(Line, Bloom, S) ->
  loadWordList(io:get_line(S, ''), etbloom:add(lists:droplast(Line), Bloom), S).
