%%%----------------------------------------------------------------------
%%% File    : mod_spam_filter.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Filter spam messages based on sender JID and content
%%% Created : 31 Mar 2019 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2019-2020 ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_spam_filter).
-author('holger@zedat.fu-berlin.de').

-behaviour(gen_server).
-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2,
	 stop/1,
	 reload/3,
	 depends/2,
	 mod_doc/0,
	 mod_opt_type/1,
	 mod_options/1]).

%% gen_server callbacks.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% ejabberd_hooks callbacks.
-export([s2s_in_handle_info/2,
	 s2s_receive_packet/1,
	 sm_receive_packet/1,
	 reopen_log/0]).

%% ejabberd_commands callbacks.
-export([get_commands_spec/0, reload_spam_filter_files/1,
	 get_spam_filter_cache/1, expire_spam_filter_cache/2,
	 drop_from_spam_filter_cache/2]).

-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(COMMAND_TIMEOUT, timer:seconds(30)).
-define(HTTPC_TIMEOUT, timer:seconds(3)).

-type url() :: binary().
-type filename() :: binary() | none.
-type jid_set() :: sets:set(ljid()).
-type url_set() :: sets:set(url()).
-type s2s_in_state() :: ejabberd_s2s_in:state().

-record(state,
	{host = <<>>          :: binary(),
	 dump_fd = undefined  :: file:io_device() | undefined,
	 url_set = sets:new() :: url_set(),
	 jid_set = sets:new() :: jid_set(),
	 jid_cache = #{}      :: map(),
	 max_cache_size = 0   :: non_neg_integer() | unlimited}).

-type state() :: #state{}.

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> ok | {error, any()}.
start(Host, Opts) ->
    ejabberd_commands:register_commands(get_commands_spec()),
    gen_mod:start_child(?MODULE, Host, Opts).

-spec stop(binary()) -> ok | {error, any()}.
stop(Host) ->
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
	false ->
	    ejabberd_commands:unregister_commands(get_commands_spec());
	true ->
	    ok
    end,
    gen_mod:stop_child(?MODULE, Host).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(Host, NewOpts, OldOpts) ->
    Proc = get_proc_name(Host),
    gen_server:cast(Proc, {reload, NewOpts, OldOpts}).

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(spam_dump_file) ->
    econf:either(
      econf:enum([none]),
      econf:binary());
mod_opt_type(spam_jids_file) ->
    econf:either(
      econf:enum([none]),
      econf:file());
mod_opt_type(spam_urls_file) ->
    econf:either(
      econf:enum([none]),
      econf:file());
mod_opt_type(access_spam) ->
    econf:acl();
mod_opt_type(cache_size) ->
    econf:pos_int(unlimited).

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(_Host) ->
    [{spam_dump_file, none},
     {spam_jids_file, none},
     {spam_urls_file, none},
     {access_spam, none},
     {cache_size, 10000}].

mod_doc() -> #{}.

%%--------------------------------------------------------------------
%% gen_server callbacks.
%%--------------------------------------------------------------------
-spec init(list()) -> {ok, state()} | {stop, term()}.
init([Host, Opts]) ->
    process_flag(trap_exit, true),
    DumpFile = expand_host(gen_mod:get_opt(spam_dump_file, Opts), Host),
    JIDsFile = gen_mod:get_opt(spam_jids_file, Opts),
    URLsFile = gen_mod:get_opt(spam_urls_file, Opts),
    try read_files(JIDsFile, URLsFile) of
	{JIDsSet, URLsSet} ->
	    ejabberd_hooks:add(s2s_in_handle_info, Host, ?MODULE,
			       s2s_in_handle_info, 90),
	    ejabberd_hooks:add(s2s_receive_packet, Host, ?MODULE,
			       s2s_receive_packet, 50),
	    ejabberd_hooks:add(sm_receive_packet, Host, ?MODULE,
			       sm_receive_packet, 50),
	    ejabberd_hooks:add(reopen_log_hook, ?MODULE,
			       reopen_log, 50),
	    DumpFd = if DumpFile == none ->
			     undefined;
			true ->
			     case filelib:ensure_dir(DumpFile) of
				 ok ->
				     ok;
				 {error, Reason} ->
				     Dirname = filename:dirname(DumpFile),
				     throw({open, Dirname, Reason})
			     end,
			     Modes = [append, raw, binary, delayed_write],
			     case file:open(DumpFile, Modes) of
				 {ok, Fd} ->
				     Fd;
				 {error, Reason1} ->
				     throw({open, DumpFile, Reason1})
			     end
		     end,
	    {ok, #state{host = Host,
			jid_set = JIDsSet,
			url_set = URLsSet,
			dump_fd = DumpFd,
			max_cache_size = gen_mod:get_opt(cache_size, Opts)}}
    catch {Op, File, Reason} when Op == open;
				  Op == read ->
	    ?CRITICAL_MSG("Cannot ~s ~s: ~s", [Op, File, format_error(Reason)]),
	    {stop, config_error}
    end.

-spec handle_call(term(), {pid(), term()}, state())
      -> {reply, {spam_filter, term()}, state()} | {noreply, state()}.
handle_call({check_jid, From}, _From, #state{jid_set = JIDsSet} = State) ->
    {Result, State1} = filter_jid(From, JIDsSet, State),
    {reply, {spam_filter, Result}, State1};
handle_call({check_body, URLs, JIDs, From}, _From,
	    #state{url_set = URLsSet, jid_set = JIDsSet} = State) ->
    {Result1, State1} = filter_body(URLs, URLsSet, From, State),
    {Result2, State2} = filter_body(JIDs, JIDsSet, From, State1),
    Result = if Result1 == spam ->
		     Result1;
		true ->
		     Result2
	     end,
    {reply, {spam_filter, Result}, State2};
handle_call({resolve_redirects, URLs}, _From, State) ->
    ResolvedURLs = do_resolve_redirects(URLs, []),
    {reply, {spam_filter, ResolvedURLs}, State};
handle_call({reload_files, JIDsFile, URLsFile}, _From, State) ->
    {Result, State1} = reload_files(JIDsFile, URLsFile, State),
    {reply, {spam_filter, Result}, State1};
handle_call({expire_cache, Age}, _From, State) ->
    {Result, State1} = expire_cache(Age, State),
    {reply, {spam_filter, Result}, State1};
handle_call({drop_from_cache, JID}, _From, State) ->
    {Result, State1} = drop_from_cache(JID, State),
    {reply, {spam_filter, Result}, State1};
handle_call(get_cache, _From, #state{jid_cache = Cache} = State) ->
    {reply, {spam_filter, maps:to_list(Cache)}, State};
handle_call(Request, From, State) ->
    ?ERROR_MSG("Got unexpected request from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({dump, _XML}, #state{dump_fd = undefined} = State) ->
    {noreply, State};
handle_cast({dump, XML}, #state{dump_fd = Fd} = State) ->
    case file:write(Fd, [XML, <<$\n>>]) of
	ok ->
	    ok;
	{error, Reason} ->
	    ?ERROR_MSG("Cannot write spam to dump file: ~s",
		       [file:format_error(Reason)])
    end,
    {noreply, State};
handle_cast({reload, NewOpts, OldOpts}, #state{host = Host} = State) ->
    State1 = case {gen_mod:get_opt(spam_dump_file, OldOpts),
		   gen_mod:get_opt(spam_dump_file, NewOpts)} of
		 {OldDumpFile, NewDumpFile} when NewDumpFile /= OldDumpFile ->
		     close_dump_file(expand_host(OldDumpFile, Host), State),
		     open_dump_file(expand_host(NewDumpFile, Host), State);
		 {_OldDumpFile, _NewDumpFile} ->
		     State
	     end,
    State2 = case {gen_mod:get_opt(cache_size, OldOpts),
		   gen_mod:get_opt(cache_size, NewOpts)} of
		 {OldMax, NewMax} when NewMax < OldMax ->
		     shrink_cache(State1#state{max_cache_size = NewMax});
		 {OldMax, NewMax} when NewMax > OldMax ->
		     State1#state{max_cache_size = NewMax};
		 {_OldMax, _NewMax} ->
		     State1
	     end,
    JIDsFile = gen_mod:get_opt(spam_jids_file, NewOpts),
    URLsFile = gen_mod:get_opt(spam_urls_file, NewOpts),
    {_Result, State3} = reload_files(JIDsFile, URLsFile, State2),
    {noreply, State3};
handle_cast(reopen_log, State) ->
    {noreply, reopen_dump_file(State)};
handle_cast(Request, State) ->
    ?ERROR_MSG("Got unexpected request from: ~p", [Request]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
    ?ERROR_MSG("Got unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> ok.
terminate(Reason, #state{host = Host} = State) ->
    ?DEBUG("Stopping spam filter process for ~s: ~p", [Host, Reason]),
    DumpFile = gen_mod:get_module_opt(Host, ?MODULE, spam_dump_file),
    DumpFile1 = expand_host(DumpFile, Host),
    close_dump_file(DumpFile1, State),
    ejabberd_hooks:delete(s2s_receive_packet, Host, ?MODULE,
			  s2s_receive_packet, 50),
    ejabberd_hooks:delete(sm_receive_packet, Host, ?MODULE,
			  sm_receive_packet, 50),
    ejabberd_hooks:delete(s2s_in_handle_info, Host, ?MODULE,
			  s2s_in_handle_info, 90),
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
	false ->
	    ejabberd_hooks:delete(reopen_log_hook, ?MODULE,
				  reopen_log, 50);
	true ->
	    ok
    end.

-spec code_change({down, term()} | term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, #state{host = Host} = State, _Extra) ->
    ?DEBUG("Updating spam filter process for ~s", [Host]),
    {ok, State}.

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------
-spec s2s_receive_packet({stanza() | drop, s2s_in_state()})
      -> {stanza() | drop, s2s_in_state()} | {stop, {drop, s2s_in_state()}}.
s2s_receive_packet({A, State}) ->
    case sm_receive_packet(A) of
        {stop, drop} ->
            {stop, {drop, State}};
        Result ->
            {Result, State}
    end.

-spec sm_receive_packet(stanza() | drop) -> stanza() | drop | {stop, drop}.
sm_receive_packet(drop = Acc) ->
    Acc;
sm_receive_packet(#message{from = From,
			     to = #jid{lserver = LServer} = To,
			     type = Type, body = Body} = Msg
		    = Acc) when Type /= groupchat,
				       Type /= error ->
    case needs_checking(From, To) of
	true ->
	    case check_from(LServer, From) of
		ham ->
		    case check_body(LServer, From, xmpp:get_text(Body)) of
			ham ->
			    Acc;
			spam ->
			    reject(Msg),
			    {stop, drop}
		    end;
		spam ->
		    reject(Msg),
		    {stop, drop}
	    end;
	false ->
	    Acc
    end;
sm_receive_packet(#presence{from = From,
			      to = #jid{lserver = LServer} = To,
			      type = subscribe} = Presence = Acc) ->
    case needs_checking(From, To) of
	true ->
	    case check_from(LServer, From) of
		ham ->
		    Acc;
		spam ->
		    reject(Presence),
		    {stop, drop}
	    end;
	false ->
	    Acc
    end;
sm_receive_packet(Acc) ->
    Acc.

-spec s2s_in_handle_info(s2s_in_state(), any())
      -> s2s_in_state() | {stop, s2s_in_state()}.
s2s_in_handle_info(State, {_Ref, {spam_filter, _}}) ->
    ?DEBUG("Dropping expired spam filter result", []),
    {stop, State};
s2s_in_handle_info(State, _) ->
    State.

-spec reopen_log() -> ok.
reopen_log() ->
    lists:foreach(fun(Host) ->
			  Proc = get_proc_name(Host),
			  gen_server:cast(Proc, reopen_log)
		  end, get_spam_filter_hosts()).

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec needs_checking(jid(), jid()) -> boolean().
needs_checking(From, #jid{lserver = LServer} = To) ->
    case gen_mod:is_loaded(LServer, ?MODULE) of
	true ->
	    Access = gen_mod:get_module_opt(LServer, ?MODULE, access_spam),
	    case acl:match_rule(LServer, Access, To) of
		allow ->
		    ?DEBUG("Spam not filtered for ~s", [jid:encode(To)]),
		    false;
		deny ->
		    ?DEBUG("Spam is filtered for ~s", [jid:encode(To)]),
		    not mod_roster:is_subscribed(From, To)
	    end;
	false ->
	    ?DEBUG("~s not loaded for ~s", [?MODULE, LServer]),
	    false
    end.

-spec check_from(binary(), jid()) -> ham | spam.
check_from(Host, From) ->
    Proc = get_proc_name(Host),
    LFrom = jid:remove_resource(jid:tolower(From)),
    try gen_server:call(Proc, {check_jid, LFrom}) of
	{spam_filter, Result} ->
	    Result
    catch exit:{timeout, _} ->
	    ?WARNING_MSG("Timeout while checking ~s against list of spammers",
			 [jid:encode(From)]),
	    ham
    end.

-spec check_body(binary(), jid(), binary()) -> ham | spam.
check_body(Host, From, Body) ->
    case {extract_urls(Host, Body), extract_jids(Body)} of
	{none, none} ->
	    ?DEBUG("No JIDs/URLs found in message", []),
	    ham;
	{URLs, JIDs} ->
	    Proc = get_proc_name(Host),
	    LFrom = jid:remove_resource(jid:tolower(From)),
	    try gen_server:call(Proc, {check_body, URLs, JIDs, LFrom}) of
		{spam_filter, Result} ->
		    Result
	    catch exit:{timeout, _} ->
		    ?WARNING_MSG("Timeout while checking body", []),
		    ham
	    end
    end.

-spec extract_urls(binary(), binary()) -> {urls, [url()]} | none.
extract_urls(Host, Body) ->
    RE = <<"https?://\\S+">>,
    Options = [global, {capture, all, binary}],
    case re:run(Body, RE, Options) of
	{match, Captured} when is_list(Captured) ->
      Urls = resolve_redirects(Host, lists:flatten(Captured)),
	    {urls, Urls};
	nomatch ->
	    none
    end.

-spec resolve_redirects(binary(), [url()]) -> [url()].
resolve_redirects(Host, URLs) ->
    Proc = get_proc_name(Host),
    try gen_server:call(Proc, {resolve_redirects, URLs}) of
        {spam_filter, ResolvedURLs} ->
            ResolvedURLs
    catch exit:{timeout, _} ->
            ?WARNING_MSG("Timeout while resolving redirects: ~p", [URLs]),
            URLs
    end.

-spec do_resolve_redirects([url()], [url()]) -> [url()].
do_resolve_redirects([], Result) -> Result;
do_resolve_redirects([URL | Rest], Acc) ->
    case
        httpc:request(get, {URL, [{"user-agent", "curl/8.7.1"}]},
                      [{autoredirect, false}, {timeout, ?HTTPC_TIMEOUT}], [])
    of
        {ok, {{_, StatusCode, _}, Headers, _Body}} when StatusCode >= 300, StatusCode < 400 ->
            Location = proplists:get_value("location", Headers),
            case Location == undefined orelse lists:member(Location, Acc) of
                true ->
                    do_resolve_redirects(Rest, [URL | Acc]);
                false ->
                    do_resolve_redirects([Location | Rest], [URL | Acc])
              end;
        _Res ->
            do_resolve_redirects(Rest, [URL | Acc])
    end.

-spec extract_jids(binary()) -> {jids, [ljid()]} | none.
extract_jids(Body) ->
    RE = <<"\\S+@\\S+">>,
    Options = [global, {capture, all, binary}],
    case re:run(Body, RE, Options) of
	{match, Captured} when is_list(Captured) ->
	    {jids, lists:filtermap(fun try_decode_jid/1,
				   lists:flatten(Captured))};
	nomatch ->
	    none
    end.

-spec try_decode_jid(binary()) -> {true, ljid()} | false.
try_decode_jid(S) ->
    try jid:decode(S) of
	#jid{} = JID ->
	    {true, jid:remove_resource(jid:tolower(JID))}
    catch _:{bad_jid, _} ->
	    false
    end.

-spec filter_jid(ljid(), jid_set(), state()) -> {ham | spam, state()}.
filter_jid(From, Set, State) ->
    case sets:is_element(From, Set) of
	true ->
	    ?DEBUG("Spam JID found: ~s", [jid:encode(From)]),
	    {spam, State};
	false ->
	    case cache_lookup(From, State) of
		{true, State1} ->
		    ?DEBUG("Spam JID found: ~s", [jid:encode(From)]),
		    {spam, State1};
		{false, State1} ->
		    ?DEBUG("JID not listed: ~s", [jid:encode(From)]),
		    {ham, State1}
	    end
    end.

-spec filter_body({urls, [url()]} | {jids, [ljid()]} | none,
		  url_set() | jid_set(), jid(), state())
      -> {ham | spam, state()}.
filter_body({_, Addrs}, Set, From, State) ->
    case lists:any(fun(Addr) -> sets:is_element(Addr, Set) end, Addrs) of
	true ->
	    ?DEBUG("Spam addresses found: ~p", [Addrs]),
	    {spam, cache_insert(From, State)};
	false ->
	    ?DEBUG("Addresses not listed: ~p", [Addrs]),
	    {ham, State}
    end;
filter_body(none, _Set, _From, State) ->
    {ham, State}.

-spec reload_files(filename(), filename(), state())
      -> {ok | {error, binary()}, state()}.
reload_files(JIDsFile, URLsFile, #state{host = Host} = State) ->
    try read_files(JIDsFile, URLsFile) of
	{JIDsSet, URLsSet} ->
	    case sets_equal(JIDsSet, State#state.jid_set) of
		true ->
		    ?INFO_MSG("Reloaded spam JIDs for ~s (unchanged)", [Host]);
		false ->
		    ?INFO_MSG("Reloaded spam JIDs for ~s (changed)", [Host])
	    end,
	    case sets_equal(URLsSet, State#state.url_set) of
		true ->
		    ?INFO_MSG("Reloaded spam URLs for ~s (unchanged)", [Host]);
		false ->
		    ?INFO_MSG("Reloaded spam URLs for ~s (changed)", [Host])
	    end,
	    {ok, State#state{jid_set = JIDsSet, url_set = URLsSet}}
    catch {Op, File, Reason} when Op == open;
				  Op == read ->
	    Txt = format("Cannot ~s ~s for ~s: ~s",
			 [Op, File, Host, format_error(Reason)]),
	    ?ERROR_MSG("~s", [Txt]),
	    {{error, Txt}, State}
    end.

-spec read_files(filename(), filename()) -> {jid_set(), url_set()}.
read_files(JIDsFile, URLsFile) ->
    {read_file(JIDsFile, fun parse_jid/1),
     read_file(URLsFile, fun parse_url/1)}.

-spec read_file(filename(), fun((binary()) -> ljid() | url()))
      -> jid_set() | url_set().
read_file(none, _ParseLine) ->
    sets:new();
read_file(File, ParseLine) ->
    case file:open(File, [read, binary, raw, {read_ahead, 65536}]) of
	{ok, Fd} ->
	    try read_line(Fd, ParseLine, sets:new())
	    catch throw:E -> throw({read, File, E})
	    after ok = file:close(Fd)
	    end;
	{error, Reason} ->
	    throw({open, File, Reason})
    end.

-spec read_line(file:io_device(), fun((binary()) -> ljid() | url()),
		jid_set() | url_set())
      -> jid_set() | url_set().
read_line(Fd, ParseLine, Set) ->
    case file:read_line(Fd) of
	{ok, Line} ->
	    read_line(Fd, ParseLine, sets:add_element(ParseLine(Line), Set));
	{error, Reason} ->
	    throw(Reason);
	eof ->
	    Set
    end.

-spec parse_jid(binary()) -> ljid().
parse_jid(S) ->
    try jid:decode(trim(S)) of
	#jid{} = JID ->
	    jid:remove_resource(jid:tolower(JID))
    catch _:{bad_jid, _} ->
	    throw({bad_jid, S})
    end.

-spec parse_url(binary()) -> url().
parse_url(S) ->
    URL = trim(S),
    RE = <<"https?://\\S+$">>,
    Options = [anchored, caseless, {capture, none}],
    case re:run(URL, RE, Options) of
	match ->
	    URL;
	nomatch ->
	    throw({bad_url, S})
    end.

-spec trim(binary()) -> binary().
trim(S) ->
    re:replace(S, <<"\\s+$">>, <<>>, [{return, binary}]).

-spec reject(stanza()) -> ok.
reject(#message{from = From, to = To, type = Type, lang = Lang} = Msg)
  when Type /= groupchat,
       Type /= error ->
    ?INFO_MSG("Rejecting unsolicited message from ~s to ~s",
	      [jid:encode(From), jid:encode(To)]),
    Txt = <<"Your message is unsolicited">>,
    Err = xmpp:err_policy_violation(Txt, Lang),
    maybe_dump_spam(Msg),
    ejabberd_router:route_error(Msg, Err);
reject(#presence{from = From, to = To, lang = Lang} = Presence) ->
    ?INFO_MSG("Rejecting unsolicited presence from ~s to ~s",
	      [jid:encode(From), jid:encode(To)]),
    Txt = <<"Your traffic is unsolicited">>,
    Err = xmpp:err_policy_violation(Txt, Lang),
    ejabberd_router:route_error(Presence, Err);
reject(_) ->
    ok.

-spec open_dump_file(filename(), state()) -> state().
open_dump_file(none, State) ->
    State#state{dump_fd = undefined};
open_dump_file(Name, State) ->
    Modes = [append, raw, binary, delayed_write],
    case file:open(Name, Modes) of
	{ok, Fd} ->
	    ?DEBUG("Opened ~s", [Name]),
	    State#state{dump_fd = Fd};
	{error, Reason} ->
	    ?ERROR_MSG("Cannot open ~s: ~s", [Name, file:format_error(Reason)]),
	    State#state{dump_fd = undefined}
    end.

-spec close_dump_file(filename(), state()) -> ok.
close_dump_file(_Name, #state{dump_fd = undefined}) ->
    ok;
close_dump_file(Name, #state{dump_fd = Fd}) ->
    case file:close(Fd) of
	ok ->
	    ?DEBUG("Closed ~s", [Name]);
	{error, Reason} ->
	    ?ERROR_MSG("Cannot close ~s: ~s", [Name, file:format_error(Reason)])
    end.

-spec reopen_dump_file(state()) -> state().
reopen_dump_file(#state{host = Host} = State) ->
    DumpFile = gen_mod:get_module_opt(Host, ?MODULE, spam_dump_file),
    DumpFile1 = expand_host(DumpFile, Host),
    close_dump_file(DumpFile1, State),
    open_dump_file(DumpFile1, State).

-spec maybe_dump_spam(message()) -> ok.
maybe_dump_spam(#message{to = #jid{lserver = LServer}} = Msg) ->
    By = jid:make(<<>>, LServer),
    Proc = get_proc_name(LServer),
    Time = erlang:timestamp(),
    Msg1 = misc:add_delay_info(Msg, By, Time),
    XML = fxml:element_to_binary(xmpp:encode(Msg1)),
    gen_server:cast(Proc, {dump, XML}).

-spec get_proc_name(binary()) -> atom().
get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

-spec get_spam_filter_hosts() -> [binary()].
get_spam_filter_hosts() ->
    [H || H <- ejabberd_option:hosts(), gen_mod:is_loaded(H, ?MODULE)].

-spec expand_host(binary() | none, binary()) -> binary() | none.
expand_host(none, _Host) ->
    none;
expand_host(Input, Host) ->
    misc:expand_keyword(<<"@HOST@">>, Input, Host).

-spec sets_equal(sets:set(), sets:set()) -> boolean().
sets_equal(A, B) ->
    sets:is_subset(A, B) andalso sets:is_subset(B, A).

-spec format(io:format(), [term()]) -> binary().
format(Format, Data) ->
    iolist_to_binary(io_lib:format(Format, Data)).

-spec format_error(atom() | tuple()) -> binary().
format_error({bad_jid, JID}) ->
    <<"Not a valid JID: ", JID/binary>>;
format_error({bad_url, URL}) ->
    <<"Not an HTTP(S) URL: ", URL/binary>>;
format_error(Reason) ->
    list_to_binary(file:format_error(Reason)).

%%--------------------------------------------------------------------
%% Caching.
%%--------------------------------------------------------------------
-spec cache_insert(ljid(), state()) -> state().
cache_insert(_LJID, #state{max_cache_size = 0} = State) ->
    State;
cache_insert(LJID, #state{jid_cache = Cache, max_cache_size = MaxSize} = State)
  when MaxSize /= unlimited, map_size(Cache) >= MaxSize ->
    cache_insert(LJID, shrink_cache(State));
cache_insert(LJID, #state{jid_cache = Cache} = State) ->
    ?INFO_MSG("Caching spam JID: ~s", [jid:encode(LJID)]),
    Cache1 = Cache#{LJID => erlang:monotonic_time(second)},
    State#state{jid_cache = Cache1}.

-spec cache_lookup(ljid(), state()) -> {boolean(), state()}.
cache_lookup(LJID, #state{jid_cache = Cache} = State) ->
    case Cache of
	#{LJID := _Timestamp} ->
	    Cache1 = Cache#{LJID => erlang:monotonic_time(second)},
	    State1 = State#state{jid_cache = Cache1},
	    {true, State1};
	#{} ->
	    {false, State}
    end.

-spec shrink_cache(state()) -> state().
shrink_cache(#state{jid_cache = Cache, max_cache_size = MaxSize} = State) ->
    ShrinkedSize = round(MaxSize / 2),
    N = map_size(Cache) - ShrinkedSize,
    L = lists:keysort(2, maps:to_list(Cache)),
    Cache1 = maps:from_list(lists:nthtail(N, L)),
    State#state{jid_cache = Cache1}.

-spec expire_cache(integer(), state()) -> {{ok, binary()}, state()}.
expire_cache(Age, #state{jid_cache = Cache} = State) ->
    Threshold = erlang:monotonic_time(second) - Age,
    Cache1 = maps:filter(fun(_, TS) -> TS >= Threshold end, Cache),
    NumExp = map_size(Cache) - map_size(Cache1),
    Txt = format("Expired ~B cache entries", [NumExp]),
    {{ok, Txt}, State#state{jid_cache = Cache1}}.

-spec drop_from_cache(ljid(), state()) -> {{ok, binary()}, state()}.
drop_from_cache(LJID, #state{jid_cache = Cache} = State) ->
    Cache1 = maps:remove(LJID, Cache),
    if map_size(Cache1) < map_size(Cache) ->
	    Txt = format("~s removed from cache", [jid:encode(LJID)]),
	    {{ok, Txt}, State#state{jid_cache = Cache1}};
       true ->
	    Txt = format("~s wasn't cached", [jid:encode(LJID)]),
	    {{ok, Txt}, State}
    end.

%%--------------------------------------------------------------------
%% ejabberd command callbacks.
%%--------------------------------------------------------------------
-spec get_commands_spec() -> [ejabberd_commands()].
get_commands_spec() ->
    [#ejabberd_commands{name = reload_spam_filter_files, tags = [filter],
			desc = "Reload spam JID/URL files",
			module = ?MODULE, function = reload_spam_filter_files,
			args = [{host, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = get_spam_filter_cache, tags = [filter],
			desc = "Show spam filter cache contents",
			module = ?MODULE, function = get_spam_filter_cache,
			args = [{host, binary}],
			result = {spammers, {list, {spammer, {tuple,
				  [{jid, string}, {timestamp, integer}]}}}}},
     #ejabberd_commands{name = expire_spam_filter_cache, tags = [filter],
			desc = "Remove old/unused spam JIDs from cache",
			module = ?MODULE, function = expire_spam_filter_cache,
			args = [{host, binary}, {seconds, integer}],
			result = {res, restuple}},
     #ejabberd_commands{name = drop_from_spam_filter_cache, tags = [filter],
			desc = "Drop JID from spam filter cache",
			module = ?MODULE,
			function = drop_from_spam_filter_cache,
			args = [{host, binary}, {jid, binary}],
			result = {res, restuple}}].

-spec reload_spam_filter_files(binary()) -> ok | {error, string()}.
reload_spam_filter_files(<<"global">>) ->
    try lists:foreach(fun(Host) ->
			      ok = reload_spam_filter_files(Host)
		      end, get_spam_filter_hosts())
    catch error:{badmatch, {error, _Reason} = Error} ->
	    Error
    end;
reload_spam_filter_files(Host) ->
    LServer = jid:nameprep(Host),
    case {gen_mod:get_module_opt(LServer, ?MODULE, spam_jids_file),
	  gen_mod:get_module_opt(LServer, ?MODULE, spam_urls_file)} of
	{JIDsFile, URLsFile} ->
	    Proc = get_proc_name(LServer),
	    try gen_server:call(Proc, {reload_files, JIDsFile, URLsFile},
				?COMMAND_TIMEOUT) of
		{spam_filter, ok} ->
		    ok;
		{spam_filter, {error, Txt}} ->
		    {error, binary_to_list(Txt)}
	    catch exit:{noproc, _} ->
		    {error, "Not configured for " ++ binary_to_list(Host)};
		  exit:{timeout, _} ->
		    {error, "Timeout while querying ejabberd"}
	    end
    end.

-spec get_spam_filter_cache(binary())
      -> [{binary(), integer()}] | {error, string()}.
get_spam_filter_cache(Host) ->
    LServer = jid:nameprep(Host),
    Proc = get_proc_name(LServer),
    try gen_server:call(Proc, get_cache, ?COMMAND_TIMEOUT) of
	{spam_filter, Cache} ->
	    [{jid:encode(JID), TS + erlang:time_offset(second)} ||
	     {JID, TS} <- Cache]
    catch exit:{noproc, _} ->
	    {error, "Not configured for " ++ binary_to_list(Host)};
	  exit:{timeout, _} ->
	    {error, "Timeout while querying ejabberd"}
    end.

-spec expire_spam_filter_cache(binary(), integer()) -> {ok | error, string()}.
expire_spam_filter_cache(<<"global">>, Age) ->
    try lists:foreach(fun(Host) ->
			      {ok, _} = expire_spam_filter_cache(Host, Age)
		      end, get_spam_filter_hosts()) of
	ok ->
	    {ok, "Expired cache entries"}
    catch error:{badmatch, {error, _Reason} = Error} ->
	    Error
    end;
expire_spam_filter_cache(Host, Age) ->
    LServer = jid:nameprep(Host),
    Proc = get_proc_name(LServer),
    try gen_server:call(Proc, {expire_cache, Age}, ?COMMAND_TIMEOUT) of
	{spam_filter, {Status, Txt}} ->
	    {Status, binary_to_list(Txt)}
    catch exit:{noproc, _} ->
	    {error, "Not configured for " ++ binary_to_list(Host)};
	  exit:{timeout, _} ->
	    {error, "Timeout while querying ejabberd"}
    end.

-spec drop_from_spam_filter_cache(binary(), binary()) -> {ok | error, string()}.
drop_from_spam_filter_cache(<<"global">>, JID) ->
    try lists:foreach(fun(Host) ->
			      {ok, _} = drop_from_spam_filter_cache(Host, JID)
		      end, get_spam_filter_hosts()) of
	ok ->
	    {ok, "Dropped " ++ binary_to_list(JID) ++ " from caches"}
    catch error:{badmatch, {error, _Reason} = Error} ->
	    Error
    end;
drop_from_spam_filter_cache(Host, EncJID) ->
    LServer = jid:nameprep(Host),
    Proc = get_proc_name(LServer),
    try jid:decode(EncJID) of
	#jid{} = JID ->
	    LJID = jid:remove_resource(jid:tolower(JID)),
	    try gen_server:call(Proc, {drop_from_cache, LJID},
				?COMMAND_TIMEOUT) of
		{spam_filter, {Status, Txt}} ->
		    {Status, binary_to_list(Txt)}
	    catch exit:{noproc, _} ->
		    {error, "Not configured for " ++ binary_to_list(Host)};
		  exit:{timeout, _} ->
		    {error, "Timeout while querying ejabberd"}
	    end
    catch _:{bad_jid, _} ->
	    {error, "Not a valid JID: " ++ binary_to_list(EncJID)}
    end.
