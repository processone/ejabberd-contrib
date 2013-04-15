%%%-------------------------------------------------------------------
%%% File    : mod_ecomm_test.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Simple commands for testing
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(mod_ecomm_test).
-author('badlop@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 %% Take: test arguments
	 take_integer/1,
	 take_string/1,
	 take_integer_string/2,
	 take_tuple_2integer/1,
	 take_tuple_2string/1,
	 take_list_integer/1,
	 take_list_string/1,
	 %% Echo: test arguments and result
	 echo_integer/1,
	 echo_string/1,
	 echo_integer_string/2,
	 echo_list_integer/1,
	 echo_list_string/1,
	 echo_integer_list_string/2,
	 echo_isatils/4,
	 %% Tell: test result
	 tell_atom/1,
	 tell_rescode/1,
	 tell_restuple/1,
	 tell_tuple_3integer/0,
	 tell_tuple_3string/0,
	 tell_tuple_3atom/0,
	 tell_tuple_3list/0,
	 tell_list_3integer/0,
	 tell_list_3string/0,
	 tell_list_3atom/0,
	 tell_list_3tuple/0,
	 %% Realistic
	 this_crashes/1,
	 this_wrong_args/1,
	 this_wrong_return/0,
	 pow/2, seq/2, substrs/1, splitjid/1, splitjids/1
	]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("jlib.hrl").

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(commands()).

%%%
%%% ejabberd commands
%%%

commands() ->
    [

     #ejabberd_commands{name = take_integer, tags = [test],
		       desc = "Take Integer in args, give Integer zero",
		       module = ?MODULE, function = take_integer,
		       args = [{thisinteger, integer}],
		       result = {zero, integer}},

     #ejabberd_commands{name = take_string, tags = [test],
		       desc = "Take String, give Integer zero",
		       module = ?MODULE, function = take_string,
		       args = [{thisstring, string}],
		       result = {zero, integer}},

     #ejabberd_commands{name = take_integer_string, tags = [test],
		       desc = "Take integer and string, give Integer zero",
		       module = ?MODULE, function = take_integer_string,
		       args = [{thisinteger, integer}, {thisstring, string}],
		       result = {zero, integer}},

     %% Not supported by ejabberd_ctl
     #ejabberd_commands{name = take_tuple_2integer, tags = [test],
		       desc = "Take Tuple of two integers, give Integer zero",
		       module = ?MODULE, function = take_tuple_2integer,
		       args = [{thistuple, {tuple, [{thisinteger1, integer}, {thisinteger2, integer}]}}],
		       result = {zero, integer}},
     %% Not supported by ejabberd_ctl
     #ejabberd_commands{name = take_tuple_2string, tags = [test],
		       desc = "Take Tuple of two strings, give Integer zero",
		       module = ?MODULE, function = take_tuple_2string,
		       args = [{thistuple, {tuple, [{thisstring1, string}, {thisstring2, string}]}}],
		       result = {zero, integer}},
     %% Not supported by ejabberd_ctl
     #ejabberd_commands{name = take_list_integer, tags = [test],
		       desc = "Take List of integers, give Integer zero",
		       module = ?MODULE, function = take_list_integer,
		       args = [{thislist, {list, {thisinteger, integer}}}],
		       result = {zero, integer}},
     %% Not supported by ejabberd_ctl
     #ejabberd_commands{name = take_list_string, tags = [test],
		       desc = "Take List of strings, give Integer zero",
		       module = ?MODULE, function = take_list_string,
		       args = [{thislist, {list, {thisstring, string}}}],
		       result = {zero, integer}},

     #ejabberd_commands{name = echo_integer, tags = [test],
		       desc = "Echo Integer",
		       module = ?MODULE, function = echo_integer,
		       args = [{thisinteger, integer}],
		       result = {thatinteger, integer}},
     #ejabberd_commands{name = echo_string, tags = [test],
		       desc = "Echo String",
		       module = ?MODULE, function = echo_string,
		       args = [{thisstring, string}],
		       result = {thatstring, string}},
     #ejabberd_commands{name = echo_integer_string, tags = [test],
		       desc = "Echo integer and string, in result as a tuple",
		       module = ?MODULE, function = echo_integer_string,
		       args = [{thisinteger, integer}, {thisstring, string}],
		       result = {thistuple, {tuple, [{thisinteger, integer}, {thisstring, string}]}}},
     %% Not supported by ejabberd_ctl
     #ejabberd_commands{name = echo_list_integer, tags = [test],
		       desc = "Echo List of integers",
		       module = ?MODULE, function = echo_list_integer,
		       args = [{thislist, {list, {thisinteger, integer}}}],
		       result = {thatlist, {list, {thatinteger, integer}}}},
     %% Not supported by ejabberd_ctl
     #ejabberd_commands{name = echo_list_string, tags = [test],
		       desc = "Echo List of strings",
		       module = ?MODULE, function = echo_list_string,
		       args = [{thislist, {list, {thisstring, string}}}],
		       result = {thatlist, {list, {thatstring, string}}}},
     %% Not supported by ejabberd_ctl
     #ejabberd_commands{name = echo_integer_list_string, tags = [test],
		       desc = "Echo an integer and List of strings",
		       module = ?MODULE, function = echo_integer_list_string,
		       args = [{thisinteger, integer}, {thislist, {list, {thisstring, string}}}],
		       result = {thistuple, {tuple, [{thatinteger, integer}, {thatlist, {list, {thatstring, string}}}]}}},
     %% Not supported by ejabberd_ctl
     #ejabberd_commands{name = echo_isatils, tags = [test],
		       desc = "Echo integer, string, atom and tuple of integer and list of strings",
		       module = ?MODULE, function = echo_isatils,
		       args = [{thisinteger, integer},
			       {thisstring, string},
			       {thisatom, atom},
			       {thistuple, {tuple, [
						    {listlen, integer},
						    {thislist, {list, {contentstring, string}}}
						   ]}}
			      ],
		       result = {results, {tuple, [{thatinteger, integer},
						   {thatstring, string},
						   {thatatom, atom},
						   {thattuple, {tuple, [
									{listlen, integer},
									{thatlist, {list, {contentstring, string}}}
								       ]}}
						  ]}}},

     #ejabberd_commands{name = tell_atom, tags = [test],
		       desc = "Tell Atom, give Integer zero",
		       module = ?MODULE, function = tell_atom,
		       args = [{thisinteger, integer}],
		       result = {thisatom, atom}},
     #ejabberd_commands{name = tell_rescode, tags = [test],
		       desc = "Tell rescode",
		       module = ?MODULE, function = tell_rescode,
		       args = [{thisinteger, integer}],
		       result = {res, rescode}},
     #ejabberd_commands{name = tell_restuple, tags = [test],
		       desc = "Tell restuple",
		       module = ?MODULE, function = tell_restuple,
		       args = [{thisinteger, integer}],
		       result = {res, restuple}},
     #ejabberd_commands{name = tell_tuple_3integer, tags = [test],
		       desc = "Tell a tuple with 3 integers",
		       module = ?MODULE, function = tell_tuple_3integer,
		       args = [],
		       result = {thattuple, {tuple, [{first, integer},
						     {second, integer},
						     {third, integer}]}}},
     #ejabberd_commands{name = tell_tuple_3string, tags = [test],
		       desc = "Tell a tuple with 3 strings",
		       module = ?MODULE, function = tell_tuple_3string,
		       args = [],
		       result = {thattuple, {tuple, [{first, string},
						     {second, string},
						     {third, string}]}}},
     #ejabberd_commands{name = tell_tuple_3atom, tags = [test],
		       desc = "Tell a tuple with 3 atoms",
		       module = ?MODULE, function = tell_tuple_3atom,
		       args = [],
		       result = {thattuple, {tuple, [{first, atom},
						     {second, atom},
						     {third, atom}]}}},
     #ejabberd_commands{name = tell_tuple_3list, tags = [test],
		       desc = "Tell a tuple with 3 lists",
		       module = ?MODULE, function = tell_tuple_3list,
		       args = [],
		       result = {thattuple, {tuple,
					     [{first, {list,
						       {thisinteger, integer}}},
					      {second, {list,
							{thisstring, string}}},
					      {third, {list,
						       {thisatom, atom}}}]}}},

     #ejabberd_commands{name = tell_list_3integer, tags = [test],
		       desc = "Tell a list with 3 integers",
		       module = ?MODULE, function = tell_list_3integer,
		       args = [],
		       result = {thatlist, {list, {thisinteger, integer}}}},
     #ejabberd_commands{name = tell_list_3string, tags = [test],
		       desc = "Tell a list with 3 strings",
		       module = ?MODULE, function = tell_list_3string,
		       args = [],
		       result = {thatlist, {list, {thisstring, string}}}},
     #ejabberd_commands{name = tell_list_3atom, tags = [test],
		       desc = "Tell a list with 3 atoms",
		       module = ?MODULE, function = tell_list_3atom,
		       args = [],
		       result = {thatlist, {list, {thisatom, atom}}}},
     #ejabberd_commands{name = tell_list_3tuple, tags = [test],
		       desc = "Tell a list with 3 tuples",
		       module = ?MODULE, function = tell_list_3tuple,
		       args = [],
		       result = {thatlist, {list, {thistuple,
						   {tuple,
						    [{thisinteger, integer},
						     {thistring, string},
						     {thisatom, atom}]}}}}},

     #ejabberd_commands{name = this_crashes, tags = [test],
		       desc = "This command crashes: test+5",
		       module = ?MODULE, function = this_crashes,
		       args = [{aninteger, integer}],
		       result = {result, integer}},
     #ejabberd_commands{name = this_wrong_args, tags = [test],
		       desc = "This problematic command defines 2 arguments but function expects 1",
		       module = ?MODULE, function = this_wrong_args,
		       args = [{a, integer}, {b, integer}],
		       result = {result, integer}},
     #ejabberd_commands{name = this_wrong_return, tags = [test],
		       desc = "This problematic command doesn't give a proper return",
		       module = ?MODULE, function = this_wrong_return,
		       args = [],
		       result = {result, integer}},

     #ejabberd_commands{name = pow, tags = [test],
			desc = "Return the power of base for exponent",
			longdesc = "This is an example command. The formula is:\n"
			" power = base ^ exponent",
			module = ?MODULE, function = pow,
			args = [{base, integer}, {exponent, integer}],
			result = {power, integer}},

     #ejabberd_commands{name = seq, tags = [test],
		       desc = "Return list of integers between two integers",
		       module = ?MODULE, function = seq,
		       args = [{from, integer}, {to, integer}],
		       result = {sequence, {list, {intermediate, integer}}}},

     #ejabberd_commands{name = substrs, tags = [test],
		       desc = "Return list of substrings of length increasing",
		       module = ?MODULE, function = substrs,
		       args = [{word, string}],
		       result = {substrings, {list, {miniword, string}}}},

     #ejabberd_commands{name = splitjid, tags = [test],
		       desc = "Split JID in parts: user, server, resource",
		       module = ?MODULE, function = splitjid,
		       args = [{jid, string}],
		       result = {jidparts, {tuple, [{user, string},
						    {server, string},
						    {resource, string}]}}},

     %% Not supported by ejabberd_ctl because uses 'list' in the arguments
     #ejabberd_commands{name = splitjids, tags = [test],
		       desc = "Split JIDs in parts: user, server, resource",
		       module = ?MODULE, function = splitjids,
		       args = [{jids, {list, {jid, string}}}],
		       result = {jidsparts,
				 {list, {jidparts,
					 {tuple, [{user, string},
						  {server, string},
						  {resource, string}]}}}}}

    ].

%%%
%%% Take
%%%

take_integer(A) when is_integer(A) -> 0.
take_string(A) when is_list(A) -> 0.
take_integer_string(A, B)
  when is_integer(A) and is_list(B) ->
    0.
take_tuple_2integer({A, B})
  when is_integer(A) and is_integer(B) ->
    0.
take_tuple_2string({A, B})
  when is_list(A) and is_list(B) ->
    0.
take_list_integer(L)
  when is_list(L) ->
    true = lists:all(fun(A) -> is_integer(A) end, L),
    0.
take_list_string(L)
  when is_list(L) ->
    true = lists:all(fun(A) -> is_list(A) end, L),
    0.

%%%
%%% Echo
%%%

echo_integer(A) when is_integer(A) -> A.
echo_string(A) when is_list(A) -> A.
echo_integer_string(A, B) when is_integer(A) and is_list(B) -> {A, B}.
echo_list_integer(L)
  when is_list(L) ->
    true = lists:all(fun(A) -> is_integer(A) end, L),
    L.
echo_list_string(L)
  when is_list(L) ->
    true = lists:all(fun(A) -> is_list(A) end, L),
    L.
echo_integer_list_string(I, L)
  when is_integer(I) and is_list(L) ->
    true = lists:all(fun(A) -> is_list(A) end, L),
    {I, L}.
echo_isatils(I, S, A, {II, L})
  when is_integer(I) and is_list(S) and is_atom(A) and is_integer(II) and is_list(L) ->
    true = lists:all(fun(SS) -> is_list(SS) end, L),
    {I, S, A, {I, L}}.


%%%
%%% Tell
%%%

tell_atom(0) -> zero;
tell_atom(1) -> one;
tell_atom(A) when is_integer(A) -> greater_than_one.

tell_rescode(0) -> ok;
tell_rescode(1) -> true;
tell_rescode(2) -> error;
tell_rescode(3) -> false;
tell_rescode(4) -> whatever.

tell_restuple(0) -> {ok, "All OK"};
tell_restuple(1) -> {true, "Successful result"};
tell_restuple(2) -> {error, "This is an error message"}.

tell_tuple_3integer() -> {123, 456, 789}.
tell_tuple_3string() -> {"Tell", "me", "a tuple please"}.
tell_tuple_3atom() -> {ok, works, perfectly}.
tell_tuple_3list() -> {[1, 23, 456], ["Tell", "me"], [all, is, ok]}.

tell_list_3integer() -> [123, 456, 789].
tell_list_3string() -> ["Tell", "me", "a tuple please"].
tell_list_3atom() -> [ok, works, perfectly].
tell_list_3tuple() ->
    [{123, "abcdefghijkl", first},
     {593, "this string", morning},
     {999, "Sleeping dog", not_seen}].


%%%
%%% Realistic
%%%

%% This function will crash for sure
this_crashes(Integer) ->
    test + Integer.

this_wrong_args(Integer) ->
    Integer + 1.

this_wrong_return() ->
    "this is a string".

pow(Base, Exponent) ->
    PowFloat = math:pow(Base, Exponent),
    round(PowFloat).

seq(From, To) ->
    lists:seq(From, To).

%% For "stick" returns: s st sti stic stick
substrs(Word) ->
    Lengths = lists:seq(1, string:len(Word)),
    [string:substr(Word, 1, Length) || Length <- Lengths].

splitjid(String) ->
    JID = jlib:string_to_jid(String),
    {JID#jid.user,
     JID#jid.server,
     JID#jid.resource}.

splitjids(Strings) ->
    [splitjid(String) || String <- Strings].

