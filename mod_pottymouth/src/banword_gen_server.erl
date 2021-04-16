-module(banword_gen_server).

-behaviour(gen_server).

-include("logger.hrl").

-export([member/1]).

%% gen_server callbacks
-export([start/1, stop/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

serverName(Lang) ->
  list_to_atom(lists:flatten([atom_to_list(?MODULE), "_", atom_to_list(Lang)])).

member({Lang, Word} = _MessageToken) ->
  gen_server:call(serverName(Lang), {member, Word}).

start({Lang, BlacklistFile} = _Opts) ->
  Name = serverName(Lang),
  ?INFO_MSG("Building blacklist name ~p~n", [Name]),
  gen_server:start_link({local, serverName(Lang)}, ?MODULE, [BlacklistFile], []).

stop() ->
    ok.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinList = binary:split(Data, [<<"\n">>], [global]),
    [binary_to_list(X) || X <- BinList].

init([BlacklistFile]) ->
  ?INFO_MSG("Building blacklist ~p~n", [BlacklistFile]),
  {ok, loadWordList(BlacklistFile)}.

check_banword(Word, BlackWord) ->
  try 
    % ?INFO_MSG("== CHECK == ~p ~p~n", [Word, BlackWord]),
    Res = string:rstr(Word, BlackWord),
    if 
      Res > 0 ->
        true;
      true ->
        false
    end
  catch _ -> 
    false
  end.

handle_call({member, Word}, _From, BlackList) ->
  % ?INFO_MSG("~p ~p~n", [Word, BlackList]),
  lists:foreach(
    fun(Elem) -> 
      Res = check_banword(Word, Elem),
      if 
        Res ->
          throw({reply, true, BlackList});
        true ->
          false          
      end
    end, BlackList),
  {reply, false, BlackList}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

loadWordList(BlacklistFile) ->
  BlacklistExists = filelib:is_file(BlacklistFile),
  if
    BlacklistExists ->
      readlines(BlacklistFile);
    true ->
      ?ERROR_MSG("Blacklist file not found: ~p~n", [BlacklistFile]),
      []
  end.
