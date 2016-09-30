-module(mod_pottymouth).

-behaviour(gen_mod).

-include("logger.hrl").

-export([
  start/2,
  stop/1,
  on_filter_packet/1,
  mod_opt_type/1,
  depends/2
]).

-include("ejabberd.hrl").

-import(bloom_gen_server, [start/0, stop/0, member/1]).
-import(nomalize_leet_gen_server, [normalize/1]).

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

censorWord({Lang, Word} = _MessageTerm) ->
  % we need unicode characters to normlize the word
  NormalizedWord = normalize_leet_gen_server:normalize({Lang, unicode:characters_to_list(list_to_binary(Word))}),
  % we need bytewise format for bloom lookup
  IsBadWord = bloom_gen_server:member({Lang, binary_to_list(unicode:characters_to_binary(NormalizedWord))}),
  if
    IsBadWord ->
      "****";
    true ->
      Word
  end.

filterWords(L) ->
  lists:map(fun censorWord/1, L).

filterMessageText(MessageAttrs, MessageText) ->
  Lang = getMessageLang(MessageAttrs),
  % we want to token-ize utf8 'words'
  MessageWords = string:tokens(unicode:characters_to_list(MessageText, utf8), " "),
  MessageTerms = [{Lang, Word} || Word <- MessageWords],
  % we get back bytewise format terms (rather than utf8)
  string:join(filterWords(MessageTerms), " ").


filterMessageBodyElements([{xmlel, <<"body">>, BodyAttr, [{xmlcdata, MessageText}]} = _H|T], MessageElements) ->
  FilteredMessageWords = binary:list_to_bin(filterMessageText(BodyAttr, binary:bin_to_list(MessageText))),
  FilteredBody = {xmlel, <<"body">>, BodyAttr, [{xmlcdata, FilteredMessageWords}]},
  filterMessageBodyElements(T, lists:append(MessageElements, [FilteredBody]));

filterMessageBodyElements([H|T], MessageElements) ->
  % skip this tag, but pass it on as processed
  filterMessageBodyElements(T, lists:append(MessageElements, [H]));

filterMessageBodyElements([], MessageElements) ->
  MessageElements.


start(_Host, Opts) ->
  Blacklists = gen_mod:get_opt(blacklists, Opts, fun(A) -> A end, []),
  lists:map(fun bloom_gen_server:start/1, Blacklists),
  CharMaps = gen_mod:get_opt(charmaps, Opts, fun(A) -> A end, []),
  lists:map(fun normalize_leet_gen_server:start/1, CharMaps),
  ejabberd_hooks:add(filter_packet, global, ?MODULE, on_filter_packet, 0),
  ok.

stop(_Host) ->
  bloom_gen_server:stop(),
  normalize_leet_gen_server:stop(),
  ejabberd_hooks:delete(filter_packet, global, ?MODULE, on_filter_packet, 0),
  ok.

on_filter_packet(drop) ->
  drop;

on_filter_packet({_From, _To, {xmlel, <<"message">>, _Attrs, Els} = _Packet} = _Msg) ->
  FilteredEls = filterMessageBodyElements(Els, []),
  {_From, _To, {xmlel, <<"message">>, _Attrs, FilteredEls}};
on_filter_packet(Msg) ->
  % Handle the generic case (any packet that isn't a message with a body).
  Msg.

mod_opt_type(blacklists) -> fun (A) when is_list(A) -> A end;
mod_opt_type(charmaps) -> fun (A) when is_list(A) -> A end;
mod_opt_type(_) -> [blacklists, charmaps].
depends(_Host, _Opts) -> [].
