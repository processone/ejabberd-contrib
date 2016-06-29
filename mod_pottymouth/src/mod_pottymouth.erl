-module(mod_pottymouth).

-behaviour(gen_mod).

-include("logger.hrl").

-export([
  start/2,
  stop/1,
  on_filter_packet/1,
  mod_opt_type/1
]).

-include("ejabberd.hrl").

-import(bloom_gen_server, [start/0, stop/0, member/1]).

getMessageLang(Attrs) ->
  LangAttr = lists:keyfind(<<"lang">>, 1, Attrs),
  if
    LangAttr ->
      {<<"lang">>, LangBin} = LangAttr,
      Lang = list_to_atom(binary_to_list(LangBin));
    true ->
      Lang = default
  end,
  Lang.

censorWord({_Lang, Word} = MessageTerm) ->
  IsBadWord = bloom_gen_server:member(MessageTerm),
  if
    IsBadWord ->
      "****";
    true ->
      Word
  end.

filterWords(L) ->
  lists:map(fun censorWord/1, L).

start(_Host, Opts) ->
  Blacklists = gen_mod:get_opt(blacklists, Opts, fun(A) -> A end, []),
  lists:map(fun bloom_gen_server:start/1, Blacklists),
  ejabberd_hooks:add(filter_packet, global, ?MODULE, on_filter_packet, 0),
  ok.

stop(_Host) ->
  bloom_gen_server:stop(),
  ejabberd_hooks:delete(filter_packet, global, ?MODULE, on_filter_packet, 0),
  ok.

on_filter_packet(drop) ->
  drop;

on_filter_packet({_From, _To, {xmlel, <<"message">>, Attrs, [_chatState, {xmlel, <<"body">>, _BodyAttr, [{xmlcdata, MessageText}] = _BodyCData} = _MessageBody] = _Els} = _Packet} = _Msg) ->
  Lang = getMessageLang(Attrs),
  MessageWords = string:tokens(binary_to_list(MessageText), " "),
  MessageTerms = [{Lang, Word} || Word <- MessageWords],
  FilteredMessageWords = list_to_binary(string:join(filterWords(MessageTerms), " ")),
  {_From, _To, {xmlel, <<"message">>, Attrs, [_chatState, {xmlel, <<"body">>, _BodyAttr, [{xmlcdata, FilteredMessageWords}]}]}};

on_filter_packet(Msg) ->
  % Handle the generic case (any packet that isn't a message with a body).
  Msg.

mod_opt_type(blacklists) -> fun (A) when is_list(A) -> A end;
mod_opt_type(_) -> [blacklists].
