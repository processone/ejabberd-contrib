-module(mod_pottymouth).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-export([
  start/2,
  stop/1,
  on_filter_packet/1,
  mod_opt_type/1,
  depends/2,
  reload/3
]).

-import(bloom_gen_server, [start/0, stop/0, member/1]).
-import(nomalize_leet_gen_server, [normalize/1]).

getMessageLang(Msg) ->
  LangAttr = xmpp:get_lang(Msg),
  if
    (LangAttr /= <<>>) ->
      Lang = list_to_atom(binary_to_list(LangAttr));
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

filterMessageText(Lang, MessageText) ->
  % we want to token-ize utf8 'words'
  MessageWords = string:tokens(unicode:characters_to_list(MessageText, utf8), " "),
  MessageTerms = [{Lang, Word} || Word <- MessageWords],
  % we get back bytewise format terms (rather than utf8)
  string:join(filterWords(MessageTerms), " ").

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

on_filter_packet(Msg) ->
  Type = xmpp:get_type(Msg),
  if 
    (Type == chat) orelse (Type == groupchat)  ->
      BodyText = xmpp:get_text(Msg#message.body),
      if
        (BodyText /= <<>>) ->
          Lang = getMessageLang(Msg),
          FilteredMessageWords = binary:list_to_bin(filterMessageText(Lang, binary:bin_to_list(BodyText))),
          [BodyObject|_] = Msg#message.body,
          NewBodyObject = setelement(3, BodyObject, FilteredMessageWords),
          NewMsg = Msg#message{body = [NewBodyObject]},
          NewMsg;
        true ->
          Msg
      end;
    true -> 
      Msg
  end.

mod_opt_type(blacklists) -> fun (A) when is_list(A) -> A end;
mod_opt_type(charmaps) -> fun (A) when is_list(A) -> A end;
mod_opt_type(_) -> [blacklists, charmaps].
depends(_Host, _Opts) -> [].
reload(_Host, _NewOpts, _OldOpts) -> ok.
