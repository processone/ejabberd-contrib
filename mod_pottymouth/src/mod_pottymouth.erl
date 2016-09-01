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
-import(nomalize_leet_gen_server, [normalize/1]).

getMessageLang(Attrs) ->
  LangAttr = lists:keyfind(<<"lang">>, 1, Attrs),
  if
    LangAttr ->
      {<<"lang">>, LangBin} = LangAttr,
      Lang = list_to_atom(binary_to_list(LangBin)),
      ?ERROR_MSG("LANG: ~p~n", [Lang]);
    true ->
      Lang = default,
      ?ERROR_MSG("LANG DEFAULT~n", [])
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
  list_to_binary(string:join(filterWords(MessageTerms), " ")).

filterMessageBodyElements([H|T]) ->
  lists:map
filterMessageBodyElements([], Element) ->
  Element.

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


% on_filter_packet({_From, _To, {xmlel, <<"message">>, _Attrs, [_chatState, {xmlel, <<"body">>, BodyAttr, [{xmlcdata, MessageText}] = _BodyCData} = _MessageBody] = _Els} = _Packet} = _Msg) ->
%  FilteredMessageWords = filterMessageText(BodyAttr, binary:bin_to_list(MessageText)),
%  {_From, _To, {xmlel, <<"message">>, _Attrs, [_chatState, {xmlel, <<"body">>, BodyAttr, [{xmlcdata, FilteredMessageWords}]}]}};
%
% on_filter_packet({_From, _To, {xmlel, <<"message">>, _Attrs, [{xmlel, <<"body">>, BodyAttr, [{xmlcdata, MessageText}] = _BodyCData} = _MessageBody] = _Els} = _Packet} = _Msg) ->
%  FilteredMessageWords = filterMessageText(BodyAttr, binary:bin_to_list(MessageText)),
%  {_From, _To, {xmlel, <<"message">>, _Attrs, [{xmlel, <<"body">>, BodyAttr, [{xmlcdata, FilteredMessageWords}]}]}};

% chat message with chat state
% on_filter_packet({_From, _To, {xmlel, <<"message">>, _Attrs, [_chatState, {xmlel, <<"body">>, BodyAttr, [{xmlcdata, MessageText}] = _BodyCData} = _MessageBody] = _Els} = _Packet} = _Msg) ->
%  ?ERROR_MSG("CHAT CHAT MSG W CHAT STATE: ~p~n", _Msg),
%  FilteredMessageWords = filterMessageText(BodyAttr, MessageText),
%  {_From, _To, {xmlel, <<"message">>, _Attrs, [_chatState, {xmlel, <<"body">>, BodyAttr, [{xmlcdata, FilteredMessageWords}]}]}};

% chat message without chat state
on_filter_packet({_From, _To, {xmlel, <<"message">>, _Attrs, [MessageBody] = _Els} = _Packet} = _Msg) ->
  ?ERROR_MSG("CHAT MSG WITHOUT CHAT STATE: ~p~n", [_Msg]),

  # {xmlel, <<"body">>, BodyAttr, [{xmlcdata, MessageText}] = _BodyCData} = _MessageBody

  if message type chat/group && !archived
    foreach body
      FilteredMessageWords = filterMessageText(BodyAttr, MessageText),
      {_From, _To, {xmlel, <<"message">>, _Attrs, [MessageBody]}]}};

on_filter_packet(Msg) ->
  % Handle the generic case (any packet that isn't a message with a body).
  ?ERROR_MSG("FILTER PACKET MSG: ~p~n", [Msg]),
  Msg.

% PSI one-to-one
% {
% {jid,<<"foo">>,<<"kalamari">>,<<"Psi">>,<<"foo">>,<<"kalamari">>,<<"Psi">>},
% {jid,<<"foo">>,<<"kalamari">>,<<>>,<<"foo">>,<<"kalamari">>,<<>>},
% {xmlel,<<"message">>,
%  [{<<"xml:lang">>,<<"en">>},{<<"type">>,<<"chat">>},{<<"to">>,<<"foo@kalamari">>},{<<"id">>,<<"aacba">>}],
%  [{xmlcdata,<<"\n">>},{xmlel,<<"body">>,[],[{xmlcdata,<<"hi">>}]},{xmlcdata,<<"\n">>},
% {xmlel,<<"active">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/chatstates">>}],[]},{xmlcdata,<<"\n">>}]}}

% gloox muc
% {
%   {jid,<<"#12345">>,<<"conference.kalamari">>,<<"bar">>,<<"#12345">>,<<"conference.kalamari">>,<<"bar">>},
%   {jid,<<"bar">>,<<"kalamari">>,<<"12145048529523376186799">>,<<"bar">>,<<"kalamari">>,<<"12145048529523376186799">>},
%   {xmlel,<<"message">>,
%     [{<<"xml:lang">>,<<"en">>},{<<"to">>,<<"#12345@conference.kalamari">>},{<<"type">>,<<"groupchat">>},{<<"from">>,<<"bar@kalamari/12145048529523376186799">>}],
%     [{xmlel,<<"archived">>,
%       [{<<"by">>,<<"conference.kalamari">>},{<<"xmlns">>,<<"urn:xmpp:mam:tmp">>},{<<"id">>,<<"1471940767114309">>}],
%       []
%   },
%   {xmlel,<<"stanza-id">>,
%     [{<<"by">>,<<"conference.kalamari">>},{<<"xmlns">>,<<"urn:xmpp:sid:0">>},{<<"id">>,<<"1471940767114309">>}],
%     []
%   },
%   {xmlel,<<"body">>,
%     [],
%     [{xmlcdata,<<"HELLO THERE">>}]
%   }]
%   }
% }
%
% {
%   {jid,<<"bar">>,<<"kalamari">>,<<"12145048529523376186799">>,<<"bar">>,<<"kalamari">>,<<"12145048529523376186799">>},
%   {jid,<<"#12345">>,<<"conference.kalamari">>,<<>>,<<"#12345">>,<<"conference.kalamari">>,<<>>},
%   {xmlel,<<"message">>,
%     [{<<"xml:lang">>,<<"en">>},{<<"to">>,<<"#12345@conference.kalamari">>},{<<"type">>,<<"groupchat">>},{<<"from">>,<<"bar@kalamari/12145048529523376186799">>}],
%     [{xmlel,<<"body">>,[],[{xmlcdata,<<"HELLO THERE">>}]}]
%   }
}%

mod_opt_type(blacklists) -> fun (A) when is_list(A) -> A end;
mod_opt_type(charmaps) -> fun (A) when is_list(A) -> A end;
mod_opt_type(_) -> [blacklists, charmaps].
