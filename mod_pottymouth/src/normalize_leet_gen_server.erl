-module(normalize_leet_gen_server).

-behaviour(gen_server).

-include("logger.hrl").

-import(normailize_leet, [normalize/2]).
-export([normalize/1]).

%% gen_server callbacks
-export([start/1, stop/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

serverName(Lang) ->
  list_to_atom(lists:flatten([atom_to_list(?MODULE), "_", atom_to_list(Lang)])).

normalize({Lang, Word} = _MessageToken) ->
  try gen_server:call(serverName(Lang), {normalize, Word})
  catch
    exit:{noproc, _Reason} -> Word
  end.

start({Lang, CharMapFile} = _Opts) ->
  gen_server:start_link({local, serverName(Lang)}, ?MODULE, [CharMapFile], []).

stop() ->
    ok.

init([CharMapFile]) ->
  ?INFO_MSG("NormalizeLeet Loading: ~p~n", [CharMapFile]),
  {ok, loadCharMapConfig(file:consult(CharMapFile))}.

loadCharMapConfig({ok, [CharMapConfig]}) ->
  maps:from_list(CharMapConfig);
loadCharMapConfig({error, Reason}) ->
  ?INFO_MSG("NormalizeLeet Error: ~p~n", [Reason]),
  maps:new().

handle_call({normalize, Word}, _From, CharMap) ->
  Reply = normalize_leet:normalize(CharMap, Word),
  {reply, Reply, CharMap}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
