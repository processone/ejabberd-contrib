-module(bloom_gen_server).

-behaviour(gen_server).

-include("logger.hrl").

-import(etbloom, [bloom/1, member/2]).
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-compile(export_all).

serverName(Lang) ->
  list_to_atom(lists:flatten([atom_to_list(?MODULE), "_", atom_to_list(Lang)])).

member({Lang, Word} = _MessageToken) ->
  gen_server:call(serverName(Lang), {member, Word}).

loadWordList(BlacklistFile) ->
  BlacklistExists = filelib:is_file(BlacklistFile),
  if
    BlacklistExists ->
      {ok, S} = file:read_file(BlacklistFile);
    true ->
      ?ERROR_MSG("Blacklist file not found: ~p~n", [BlacklistFile]),
      S = <<>>
  end,
  WordList = string:tokens(binary_to_list(S), "\n"),
  WordList.

start({Lang, BlacklistFile} = _Opts) ->
  gen_server:start_link({local, serverName(Lang)}, ?MODULE, [BlacklistFile], []).

init([BlacklistFile]) ->
  WordList = loadWordList(BlacklistFile),
  {ok, etbloom:bloom(WordList)}.

handle_call({member, Word}, _From, Bloom) ->
  Reply = etbloom:member(Word, Bloom),
  {reply, Reply, Bloom}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
