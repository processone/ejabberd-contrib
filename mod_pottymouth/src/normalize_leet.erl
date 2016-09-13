%% -*- coding: utf-8 -*-

-module(normalize_leet).

-export([
  normalize/2
]).

distinctLetters([H|T]) ->
  distinctLetters(T, [H]).

distinctLetters([H|T], Letters) ->
  distinctLetters(T, lists:umerge(Letters, [H]));
distinctLetters([], Letters) ->
  Letters.

checkMetaChar(Char) ->
  MetaChars = ["\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "{"],
  lists:member(Char, MetaChars).

replaceChar(true, Char, X, Word) ->
  re:replace(Word, ["\\", Char], X, [unicode,global,{return,list}]);
replaceChar(false, Char, X, Word) ->
  re:replace(Word, Char, X, [unicode,global,{return,list}]).

replaceLetters([H|T], CharMap, Word) ->
  CurChar = [H],
  NormChar = maps:get(CurChar, CharMap, skip),
  if
    NormChar == skip ->
      replaceLetters(T, CharMap, Word);
    true ->
      IsMetaChar = checkMetaChar(CurChar),
      replaceLetters(T, CharMap, replaceChar(IsMetaChar, CurChar, NormChar, Word))
  end;
replaceLetters([], _CharMap, Word) ->
  Word.

normalize(CharMap, Word) ->
  replaceLetters(distinctLetters(Word), CharMap, Word).
