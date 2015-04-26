%%%----------------------------------------------------------------------
%%% File    : mod_mam.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Message Archive Management (XEP-0313)
%%% Created : 25 Jan 2015 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2015   ProcessOne
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

-module(mod_mam).
-author('holger@zedat.fu-berlin.de').

-define(NS_MAM, <<"urn:xmpp:mam:0">>).
-define(PROCNAME, ?MODULE).
-define(GEN_SERVER, gen_server).
-define(DEFAULT_MAX_MESSAGES, infinity).
-define(DEFAULT_PAGE_SIZE, 25).
-define(MAX_PAGE_SIZE, 100).

-behaviour(?GEN_SERVER).
-behaviour(gen_mod).

%% gen_mod/supervisor callbacks.
-export([start_link/2,
	 start/2,
	 stop/1]).

%% gen_server callbacks.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% ejabberd_hooks callbacks.
-export([disco_features/5,
	 receive_stanza/4,
	 send_stanza/3,
	 remove_user/2]).

%% gen_iq_handler callback.
-export([handle_iq/3]).

%% Spawned processes.
-export([maybe_change_mnesia_fragment_count/2]).

-include("jlib.hrl").
-include("logger.hrl").

-record(mam_msg,
	{key                    :: mam_msg_key(),
	 time                   :: erlang:timestamp(),
	 route                  :: route(),
	 from                   :: ljid(),
	 to                     :: ljid(),
	 stanza                 :: xmlel()}).

-record(mam_meta,
	{mam_jid                :: mam_jid(),
	 mam_type               :: mam_type(),
	 first_id               :: mam_msg_id() | undefined,
	 last_id                :: mam_msg_id() | undefined,
	 arch_always = []       :: [jid()],
	 arch_never = []        :: [jid()],
	 arch_default = always  :: mam_behavior()}).

-record(mam_filter,
	{start                  :: erlang:timestamp() | error,
	 fin                    :: erlang:timestamp() | error,
	 with                   :: ljid() | error}).

-record(mam_query,
	{mam_jid                :: mam_jid(),
	 direction              :: direction(),
	 query_id               :: binary() | undefined,
	 id                     :: mam_msg_id() | undefined | error,
	 index                  :: non_neg_integer() | undefined | error,
	 max                    :: non_neg_integer() | undefined | error,
	 filter                 :: mam_filter()}).

-record(mam_query_state,
	{messages = []          :: [mam_msg()],
	 first                  :: mam_msg_id() | undefined,
	 last                   :: mam_msg_id() | undefined,
	 current                :: mam_msg_id() | undefined,
	 n_remaining            :: non_neg_integer()}).

-record(mam_result,
	{messages = []          :: [mam_msg()],
	 count                  :: non_neg_integer() | undefined,
	 index                  :: non_neg_integer() | undefined,
	 first                  :: mam_msg_id() | undefined,
	 last                   :: mam_msg_id() | undefined,
	 is_complete            :: boolean()}).

-record(mam_page_size_conf,
	{default                :: pos_integer(),
	 max                    :: pos_integer()}).

-record(state,
	{host                           :: binary(),
	 access_max_messages            :: atom(),
	 request_activates_archiving    :: boolean()}).

-type state() :: #state{}.
-type mam_msg() :: #mam_msg{}.
-type mam_meta() :: #mam_meta{}.
-type mam_filter() :: #mam_filter{}.
-type mam_query() :: #mam_query{}.
-type mam_query_state() :: #mam_query_state{}.
-type mam_result() :: #mam_result{}.
-type mam_page_size_conf() :: #mam_page_size_conf{}.
-type mam_jid() :: {binary(), binary()}.
-type mam_msg_id() :: non_neg_integer().
-type mam_msg_key() :: {mam_jid(), mam_msg_id()}.
-type mam_max_msgs() :: non_neg_integer() | infinity.
-type mam_query_id() :: binary() | undefined.
-type mam_type() :: user | muc | pubsub.
-type mam_filter_type() :: start | fin | with.
-type mam_behavior() :: always | never | roster.
-type route() :: incoming | outgoing.
-type direction() :: before | aft.
-type db_type() :: odbc | mnesia | riak. % But we currently only support Mnesia.

%%--------------------------------------------------------------------
%% gen_mod/supervisor callbacks.
%%--------------------------------------------------------------------

-spec start_link(binary(), gen_mod:opts()) -> {ok, pid()} | ignore | {error, _}.

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ?GEN_SERVER:start_link({local, Proc}, ?MODULE, {Host, Opts}, []).

-spec start(binary(), gen_mod:opts()) -> {ok, _} | {ok, _, _} | {error, _}.

start(Host, Opts) ->
    %% Set up processing of MAM requests.
    IQDisc = gen_mod:get_opt(iqdisc, Opts,
			     fun gen_iq_handler:check_type/1,
			     parallel),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM, ?MODULE,
				  handle_iq, IQDisc),
    %% Set up MAM feature announcement.
    mod_disco:register_feature(Host, ?NS_MAM),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, disco_features, 50),
    %% Set up message storage process.
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Spec = {Proc, {?MODULE, start_link, [Host, Opts]}, permanent, 3000, worker,
	    [?MODULE]},
    supervisor:start_child(ejabberd_sup, Spec).

-spec stop(binary()) -> ok.

stop(Host) ->
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, disco_features, 50),
    %% Stop message storage process.
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ok = supervisor:terminate_child(ejabberd_sup, Proc),
    ok = supervisor:delete_child(ejabberd_sup, Proc).

%%--------------------------------------------------------------------
%% gen_server callbacks.
%%--------------------------------------------------------------------

-spec init({binary(), gen_mod:opts()}) -> {ok, state()}.

init({Host, Opts}) ->
    process_flag(trap_exit, true),
    case gen_mod:db_type(Opts) of
      mnesia ->
	  %% We start with two fragments; and by default, each fragment is
	  %% replicated to all nodes.
	  FragProperties = [{n_fragments, 2},
			    {n_disc_only_copies, length([myself | nodes()])}],
	  mnesia:create_table(mam_msg,
			      [{frag_properties, FragProperties},
			       {attributes, record_info(fields, mam_msg)}]),
	  mnesia:create_table(mam_meta,
			      [{disc_copies, [node()]},
			       {attributes, record_info(fields, mam_meta)}]);
      _ -> ok
    end,
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       receive_stanza, 50),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       send_stanza, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host, ?MODULE,
		       remove_user, 50),
    AccessMaxMsgs =
	gen_mod:get_opt(access_max_user_messages, Opts,
			fun(A) when is_atom(A) -> A end, max_user_mam_messages),
    RequestActivatesArchiving =
	gen_mod:get_opt(request_activates_archiving, Opts,
			fun(B) when is_boolean(B) -> B end, true),
    {ok, #state{host = Host,
		access_max_messages = AccessMaxMsgs,
		request_activates_archiving = RequestActivatesArchiving}}.

-spec handle_call(_, {pid(), _}, state()) -> {noreply, state()}.

handle_call(Request, From, State) ->
    ?ERROR_MSG("Got unexpected request from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.

handle_cast({store, US, Msg}, State) ->
    maybe_store_message(State, US, Msg),
    {noreply, State};
handle_cast(Request, State) ->
    ?ERROR_MSG("Got unexpected request: ~p", [Request]),
    {noreply, State}.

-spec handle_info(timeout | _, state()) -> {noreply, state()}.

handle_info({'EXIT', Pid, normal}, State) ->
    ?DEBUG("Got info: PID ~w exited normally", [Pid]),
    {noreply, State};
handle_info(Info, State) ->
    ?ERROR_MSG("Got unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, _} | _, _) -> ok.

terminate(Reason, #state{host = Host}) ->
    ?DEBUG("Stopping MAM archiving for ~s: ~p", [Host, Reason]),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  receive_stanza, 50),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  send_stanza, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host, ?MODULE,
			  remove_user, 50).

-spec code_change({down, _} | _, state(), _) -> {ok, state()}.

code_change(_OldVsn, #state{host = Host} = State, _Extra) ->
    ?DEBUG("Updating MAM archiving process for ~s", [Host]),
    {ok, State}.

%%--------------------------------------------------------------------
%% ejabberd_hooks callbacks.
%%--------------------------------------------------------------------

-spec disco_features(empty | {result, [binary()]}, jid(), jid(), binary(),
		     binary()) -> {result, [binary()]}.

disco_features(empty, From, To, Node, Lang) ->
    disco_features({result, []}, From, To, Node, Lang);
disco_features({result, OtherFeatures},
	       #jid{luser = U, lserver = S},
	       #jid{luser = U, lserver = S}, <<"">>, _Lang) ->
    {result, OtherFeatures ++ [?NS_MAM]};
disco_features(Acc, _From, _To, _Node, _Lang) -> Acc.

-spec receive_stanza(jid(), jid(), jid(), xmlel()) -> ok.

receive_stanza(#jid{luser = U, lserver = S} = JID, From, To,
	       #xmlel{name = <<"message">>} = Stanza) ->
    case is_desired(incoming, JID, To, Stanza) of
      true ->
	  Proc = gen_mod:get_module_proc(S, ?PROCNAME),
	  Msg = #mam_msg{route = incoming,
			 from = jlib:jid_tolower(From),
			 to = jlib:jid_tolower(To),
			 stanza = Stanza},
	  ?GEN_SERVER:cast(Proc, {store, {U, S}, Msg});
      false ->
	  ?DEBUG("Won't archive undesired incoming stanza for ~s",
		 [jlib:jid_to_string(To)]),
	  ok
    end;
receive_stanza(_JID, _From, _To, _Stanza) -> ok.

-spec send_stanza(jid(), jid(), xmlel()) -> ok.

send_stanza(#jid{luser = U, lserver = S} = From, To,
	    #xmlel{name = <<"message">>} = Stanza) ->
    case is_desired(outgoing, From, To, Stanza) of
      true ->
	  Proc = gen_mod:get_module_proc(S, ?PROCNAME),
	  Msg = #mam_msg{route = outgoing,
			 from = jlib:jid_tolower(From),
			 to = jlib:jid_tolower(To),
			 stanza = jlib:replace_from(From, Stanza)},
	  ?GEN_SERVER:cast(Proc, {store, {U, S}, Msg});
      false ->
	  ?DEBUG("Won't archive undesired outgoing stanza from ~s",
		 [jlib:jid_to_string(From)]),
	  ok
    end;
send_stanza(_From, _To, _Stanza) -> ok.

%%--------------------------------------------------------------------
%% Check whether stanza should be stored.
%%--------------------------------------------------------------------

-spec is_desired(route(), jid(), jid(), xmlel()) -> boolean().

is_desired(Route, JID, To, Message) ->
    is_chat_or_normal_message(Message) andalso
    has_non_empty_body(Message) andalso
    not has_no_storage_hint(Message) andalso
    not is_bare_copy(Route, JID, To) andalso
    not is_resent(Message).

-spec is_chat_or_normal_message(xmlel()) -> boolean().

is_chat_or_normal_message(#xmlel{name = <<"message">>} = Message) ->
    case message_type(Message) of
      <<"chat">> ->
	  true;
      <<"normal">> ->
	  true;
      _ ->
	  false
    end;
is_chat_or_normal_message(_Message) -> false.

-spec message_type(xmlel()) -> binary().

message_type(#xmlel{attrs = Attrs}) ->
    case xml:get_attr(<<"type">>, Attrs) of
      {value, Type} ->
	  Type;
      false ->
	  <<"normal">>
    end.

-spec has_non_empty_body(xmlel()) -> boolean().

has_non_empty_body(Message) ->
    xml:get_subtag_cdata(Message, <<"body">>) =/= <<"">>.

-spec has_no_storage_hint(xmlel()) -> boolean().

has_no_storage_hint(Message) ->
    xml:get_subtag_with_xmlns(Message, <<"no-storage">>, ?NS_HINTS)
	=/= false orelse
    xml:get_subtag_with_xmlns(Message, <<"no-permanent-storage">>, ?NS_HINTS)
	=/= false.

-spec is_bare_copy(route(), jid(), jid()) -> boolean().

is_bare_copy(incoming, #jid{luser = U, lserver = S, lresource = R}, To) ->
    PrioRes = ejabberd_sm:get_user_present_resources(U, S),
    MaxRes = case catch lists:max(PrioRes) of
	       {_Prio, Res} when is_binary(Res) ->
		   Res;
	       _ ->
		   undefined
	     end,
    IsBareTo = case To of
		 #jid{lresource = <<"">>} ->
		     true;
		 #jid{lresource = LRes} ->
		     %% Unavailable resources are handled like bare JIDs.
		     lists:keyfind(LRes, 2, PrioRes) =:= false
	       end,
    case {IsBareTo, R} of
      {true, MaxRes} ->
	  ?DEBUG("Recipient of message to bare JID has top priority: ~s@~s/~s",
		 [U, S, R]),
	  false;
      {true, _R} ->
	  %% The message was sent to our bare JID, and we currently have
	  %% multiple resources with the same highest priority, so the session
	  %% manager routes the message to each of them.  We store the message
	  %% only from the resource where R equals MaxRes.
	  ?DEBUG("Additional recipient of message to bare JID: ~s@~s/~s",
		 [U, S, R]),
	  true;
      {false, _R} ->
	  false
    end;
is_bare_copy(outgoing, _JID, _To) -> false.

-spec is_resent(xmlel()) -> boolean().

is_resent(El) ->
    case xml:get_subtag_cdata(El, <<"delay">>) of
      <<"">> ->
	  false;
      Desc ->
	  binary:match(Desc, <<"Resent">>) =/= nomatch
    end.

%%--------------------------------------------------------------------
%% Store message.
%%--------------------------------------------------------------------

-spec maybe_store_message(state(), mam_jid(), mam_msg()) -> ok.

maybe_store_message(#state{host = Host} = State, US, Msg) ->
    DBType = gen_mod:db_type(Host, ?MODULE),
    maybe_store_message(State, US, Msg, DBType).

-spec maybe_store_message(state(), mam_jid(), mam_msg(), db_type()) -> ok.

maybe_store_message(#state{host = Host, access_max_messages = AccessMaxMsgs} =
		    State, {U, S} = US, Msg, mnesia) ->
    UpdateTables =
	fun() ->
		{Meta, ID} =
		    case mnesia:read(mam_meta, US, write) of
		      [#mam_meta{first_id = undefined} = M] ->
			  {M#mam_meta{first_id = 1}, 1};
		      [M] ->
			  {M, M#mam_meta.last_id + 1};
		      [] ->
			  {maybe_init_meta(State, US, mnesia), 1}
		    end,
		case Meta of
		  #mam_meta{} ->
		      case get_max_messages(AccessMaxMsgs, US, Host) of
			0 ->
			    ?DEBUG("Not storing MAM message for ~s@~s", [U, S]),
			    remove_messages(Meta, mnesia);
			MaxMsgs ->
			    ?DEBUG("Storing MAM message for ~s@~s", [U, S]),
			    store_message(Meta, ID, Msg, MaxMsgs, mnesia)
		      end;
		  undefined ->
		      ?DEBUG("Not storing MAM message for ~s@~s", [U, S]),
		      ok
		end
	end,
    Transaction = fun() -> mnesia:sync_transaction(UpdateTables) end,
    {atomic, ok} = global:trans({mod_mam_table_write, mod_mam}, Transaction),
    manage_mnesia_fragments().

-spec maybe_init_meta(state(), mam_jid(), db_type()) -> mam_meta() | undefined.

maybe_init_meta(#state{request_activates_archiving = false}, US, mnesia) ->
    #mam_meta{mam_jid = US, mam_type = user, first_id = 1};
maybe_init_meta(#state{request_activates_archiving = true}, _US, mnesia) ->
    undefined.

-spec store_message(mam_meta(), mam_msg_id(), mam_msg(), mam_max_msgs(),
		    db_type()) -> ok.

store_message(#mam_meta{mam_jid = US, first_id = FirstID, last_id = LastID} =
	      Meta, ID, Msg, MaxMsgs, mnesia)
    when MaxMsgs =:= infinity;
	 LastID =:= undefined;
	 LastID - FirstID + 1 < MaxMsgs ->
    UpdateMsgTab =
	fun() ->
		mnesia:write(Msg#mam_msg{key = {US, ID}, time = os:timestamp()})
	end,
    mnesia:activity(transaction, UpdateMsgTab, [], mnesia_frag),
    mnesia:write(Meta#mam_meta{last_id = ID});
store_message(#mam_meta{mam_jid = US, first_id = FirstID, last_id = LastID} =
	      Meta, ID, Msg, MaxMsgs, mnesia) ->
    NumStored = LastID - FirstID + 1,
    NumDelete = NumStored - MaxMsgs + 1, % + 1 to make room for the new message.
    NewFirstID = FirstID + NumDelete,
    UpdateMsgTab =
	fun() ->
		DeleteMsg = fun(DelID) ->
				    Key = {US, DelID},
				    ?DEBUG("Deleting MAM message ~w", [Key]),
				    mnesia:delete({mam_msg, Key})
			    end,
		lists:foreach(DeleteMsg, lists:seq(FirstID, NewFirstID - 1)),
		mnesia:write(Msg#mam_msg{key = {US, ID}, time = os:timestamp()})
	end,
    mnesia:activity(transaction, UpdateMsgTab, [], mnesia_frag),
    mnesia:write(Meta#mam_meta{first_id = NewFirstID, last_id = ID}).

-spec remove_messages(mam_meta(), db_type()) -> ok.

remove_messages(#mam_meta{last_id = undefined}, mnesia) -> ok;
remove_messages(#mam_meta{mam_jid = {U, S}}, mnesia) ->
    remove_user(U, S, mnesia).

-spec get_max_messages(atom(), mam_jid(), binary()) -> mam_max_msgs().

get_max_messages(AccessRule, {U, S}, Host) ->
    case acl:match_rule(Host, AccessRule, jlib:make_jid(U, S, <<"">>)) of
      Max when is_integer(Max), Max >= 0 ->
	  Max;
      infinity ->
	  infinity;
      _ ->
	  ?DEFAULT_MAX_MESSAGES
    end.

%%--------------------------------------------------------------------
%% Manage Mnesia fragments.
%%--------------------------------------------------------------------

-spec manage_mnesia_fragments() -> ok.

manage_mnesia_fragments() ->
    %% Check the table fragment sizes only occasionally.
    manage_mnesia_fragments(random:uniform(100) =:= 1).

-spec manage_mnesia_fragments(boolean()) -> ok.

manage_mnesia_fragments(true) ->
    Info = fun(Item) -> mnesia:table_info(mam_msg, Item) end,
    %%
    %% Size is the number of bytes:
    %%
    %% http://erlang.org/pipermail/erlang-questions/2009-July/044970.html
    %%
    Size = mnesia:activity(sync_dirty, Info, [memory], mnesia_frag),
    Number = mnesia:activity(sync_dirty, Info, [n_fragments], mnesia_frag),
    FragSize = Size div Number,
    ?DEBUG("Average MAM table fragment size: ~B", [FragSize]),
    %%
    %% With disc_only_copies, the (Dets) limit on each fragment is 2 GB.  We're
    %% very conservative and double the number of fragments when the average size
    %% reaches 500 MB.  See also:
    %%
    %% http://www.tamewildsystems.com/2010/05/mnesia-one-year-later-part-3.html
    %%
    Action = if FragSize > 500000000 ->
		     [add, Number];
		FragSize < 100000000, Number >= 4 ->
		     [del, Number div 2];
		true ->
		     none
	     end,
    if Action =/= none ->
	    case spawn_link(?MODULE, maybe_change_mnesia_fragment_count, Action)
		of
	      Pid when is_pid(Pid) -> ok
	    end;
       Action =:= none -> ok
    end;
manage_mnesia_fragments(false) -> ok.

-spec maybe_change_mnesia_fragment_count(add | del, pos_integer()) -> ok.

maybe_change_mnesia_fragment_count(Operation, Number) ->
    WriteLock = {mod_mam_table_write, self()},
    ReFragLock = {mod_mam_table_refrag, self()},
    ChangeFragCount =
	fun() ->
		DoIt = fun() ->
			       change_mnesia_fragment_count(Operation, Number)
		       end,
		global:trans(WriteLock, DoIt)
	end,
    case global:trans(ReFragLock, ChangeFragCount, [node() | nodes()], 0) of
      ok ->
	  ?DEBUG("Done changing number of MAM table fragments", []),
	  ok;
      aborted ->
	  ?DEBUG("Got no lock to change number of MAM table fragments", []),
	  ok
    end.

-spec change_mnesia_fragment_count(add | del, pos_integer()) -> ok.

change_mnesia_fragment_count(add, Number) ->
    ?INFO_MSG("Adding ~B table fragments for MAM storage", [Number]),
    AddFrag = fun(_N) ->
		      Info = fun(Item) -> mnesia:table_info(mam_msg, Item) end,
		      Layout = mnesia:activity(sync_dirty, Info, [frag_dist],
					       mnesia_frag),
		      {atomic, ok} =
			  mnesia:change_table_frag(mam_msg, {add_frag, Layout})
	      end,
    lists:foreach(AddFrag, lists:seq(1, Number));
change_mnesia_fragment_count(del, Number) ->
    ?INFO_MSG("Removing ~B table fragments for MAM storage", [Number]),
    DelFrag = fun(_N) ->
		      {atomic, ok} =
			  mnesia:change_table_frag(mam_msg, del_frag)
	      end,
    lists:foreach(DelFrag, lists:seq(1, Number)).

%%--------------------------------------------------------------------
%% Handle IQ requests.
%%--------------------------------------------------------------------

-spec handle_iq(jid(), jid(), iq_request()) -> iq_reply().

handle_iq(#jid{luser = U, lserver = S},
	  #jid{luser = U, lserver = S},
	  #iq{type = get, sub_el = #xmlel{name = <<"query">>}} = IQ) ->
    ?DEBUG("Got MAM form request from ~s@~s", [U, S]),
    handle_form_request(IQ);
handle_iq(#jid{luser = U, lserver = S} = From,
	  #jid{luser = U, lserver = S},
	  #iq{type = set, sub_el = #xmlel{name = <<"query">>}} = IQ) ->
    ?DEBUG("Got MAM archive request from ~s@~s", [U, S]),
    handle_archive_request(From, IQ);
handle_iq(#jid{luser = U, lserver = S}, #jid{luser = U, lserver = S},
	  #iq{sub_el = #xmlel{name = <<"prefs">>} = SubEl} = IQ) ->
    ?DEBUG("Refusing MAM preferences request from ~s@~s (not implemented)",
	   [U, S]),
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]};
handle_iq(From, To, #iq{sub_el = SubEl} = IQ) ->
    ?DEBUG("Refusing MAM request from ~s to ~s (forbidden)",
	   [jlib:jid_to_string(From), jlib:jid_to_string(To)]),
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}.

-spec handle_form_request(iq_request()) -> iq_result().

handle_form_request(IQ) ->
    Fields = [#xmlel{name = <<"field">>,
		     attrs = [{<<"type">>, <<"hidden">>},
			      {<<"var">>, <<"FORM_TYPE">>}],
		     children = [#xmlel{name = <<"value">>,
					children = [{xmlcdata, ?NS_MAM}]}]},
	      #xmlel{name = <<"field">>,
		     attrs = [{<<"type">>, <<"jid-single">>},
			      {<<"var">>, <<"with">>}]},
	      #xmlel{name = <<"field">>,
		     attrs = [{<<"type">>, <<"text-single">>},
			      {<<"var">>, <<"start">>}]},
	      #xmlel{name = <<"field">>,
		     attrs = [{<<"type">>, <<"text-single">>},
			      {<<"var">>, <<"end">>}]}],
    Form = #xmlel{name = <<"x">>,
		  attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
		  children = Fields},
    IQ#iq{type = result,
	  sub_el = [#xmlel{name = <<"query">>,
			   attrs = [{<<"xmlns">>, ?NS_MAM}],
			   children = [Form]}]}.

-spec handle_archive_request(jid(), iq_set()) -> ignore | iq_error().

handle_archive_request(#jid{luser = U, lserver = S} = JID,
		       #iq{sub_el = SubEl} = IQ) ->
    Query = parse_request(S, SubEl),
    case query_archive(Query#mam_query{mam_jid = {U, S}}) of
      #mam_result{messages = Msgs} = Result ->
	  ?DEBUG("MAM archive query for ~s successful",
		 [jlib:jid_to_string(JID)]),
	  QueryID = Query#mam_query.query_id,
	  send_iq_result(JID, IQ),
	  send_mam_messages(JID, QueryID, Msgs),
	  send_fin_message(JID, QueryID, Result),
	  ignore; % gen_iq_handler:process_iq/6 shouldn't respond.
      {error, Error} ->
	  ?DEBUG("MAM archive query for ~s returned an error: ~p",
		 [jlib:jid_to_string(JID), Error]),
	  IQ#iq{type = error, sub_el = [SubEl, Error]}
    end.

%%--------------------------------------------------------------------
%% Parse request.
%%--------------------------------------------------------------------

-spec parse_request(binary(), xmlel()) -> mam_query().

parse_request(Host, Query) ->
    RSM = case jlib:rsm_decode(Query) of
	    #rsm_in{} = R ->
		R;
	    none ->
		#rsm_in{}
	  end,
    Filter = parse_form(Query),
    Max = normalize_max(RSM#rsm_in.max, get_page_size_conf(Host)),
    Index = RSM#rsm_in.index,
    ID = case RSM#rsm_in.id of
	   undefined ->
	       undefined;
	   <<"">> ->
	       undefined;
	   Bin ->
	       case str:to_integer(Bin) of
		 {Int, _Rest} when is_integer(Int), Int > 0 ->
		     Int;
		 _ ->
		     error
	       end
	  end,
    Direction = case RSM#rsm_in.direction of
		  before ->
		      before;
		  _ ->
		      aft
	        end,
    QueryID = case xml:get_tag_attr(<<"queryid">>, Query) of
		{value, Value} ->
		    Value;
		false ->
		    undefined
	      end,
    Parsed = #mam_query{query_id = QueryID,
			direction = Direction,
			id = ID,
			index = Index,
			max = Max,
			filter = Filter},
    ?DEBUG("Got MAM query: ~p", [Parsed]),
    Parsed.

-spec parse_form(xmlel() | [xmlel()]) -> mam_filter().

parse_form(#xmlel{} = Query) ->
    case xml:get_subtag_with_xmlns(Query, <<"x">>, ?NS_XDATA) of
	#xmlel{children = Fields} ->
	    parse_form(Fields);
	false ->
	    #mam_filter{}
    end;
parse_form(Fields) when is_list(Fields) ->
    Parse =
	fun(#xmlel{name = <<"field">>,
		   attrs = Attrs,
		   children = [#xmlel{name = <<"value">>, children = [Value]}]},
	    Form) ->
		case xml:get_attr_s(<<"var">>, Attrs) of
		  <<"FORM_TYPE">> ->
		      Form;
		  <<"start">> ->
		      CData = xml:get_cdata([Value]),
		      case jlib:datetime_string_to_timestamp(CData) of
			undefined ->
			    Form#mam_filter{start = error};
			Start ->
			    Form#mam_filter{start = Start}
		      end;
		  <<"end">> ->
		      CData = xml:get_cdata([Value]),
		      case jlib:datetime_string_to_timestamp(CData) of
			undefined ->
			    Form#mam_filter{fin = error};
			End ->
			    Form#mam_filter{fin = End}
		      end;
		  <<"with">> ->
		      CData = xml:get_cdata([Value]),
		      case jlib:string_to_jid(CData) of
			error ->
			    Form#mam_filter{with = error};
			#jid{} = JID ->
			    Form#mam_filter{with = jlib:jid_tolower(JID)}
		      end;
		  Var ->
		      ?DEBUG("Got unexpected form variable: ~p", [Var]),
		      Form
		end;
	   (El, Form) ->
		?DEBUG("Got unexpected form element: ~p", [El]),
		Form
	end,
    lists:foldl(Parse, #mam_filter{}, Fields).

-spec get_page_size_conf(binary()) -> mam_page_size_conf().

get_page_size_conf(Host) ->
    DefaultPageSize = gen_mod:get_module_opt(Host, ?MODULE, default_page_size,
					     fun(D) when is_integer(D), D > 0 ->
						     D
					     end,
					     ?DEFAULT_PAGE_SIZE),
    MaxPageSize = gen_mod:get_module_opt(Host, ?MODULE, max_page_size,
					 fun(M) when is_integer(M), M > 0 ->
						 M
					 end,
					 ?MAX_PAGE_SIZE),
    #mam_page_size_conf{default = DefaultPageSize, max = MaxPageSize}.

-spec normalize_max(integer() | undefined | error, mam_page_size_conf())
      -> non_neg_integer() | error.

normalize_max(error, _Conf) ->
    error;
normalize_max(undefined, #mam_page_size_conf{default = Default}) ->
    Default;
normalize_max(Max, #mam_page_size_conf{max = Limit}) when Max > Limit ->
    Limit;
normalize_max(Max, _Conf) when Max < 0 -> % Huh.
    0;
normalize_max(Max, _Conf) ->
    Max.

-spec get_start_id(mam_query(), mam_meta()) -> mam_msg_id() | undefined.

get_start_id(_Query,
	     #mam_meta{first_id = undefined, last_id = undefined}) ->
    undefined;
get_start_id(#mam_query{id = ID, index = undefined, direction = before},
	     #mam_meta{last_id = LastID}) when ID =:= undefined;
					       ID > LastID ->
    LastID;
get_start_id(#mam_query{id = ID, index = undefined, direction = before},
	     _Meta) ->
    ID - 1;
get_start_id(#mam_query{id = ID, index = undefined, direction = aft},
	     #mam_meta{first_id = FirstID}) when ID =:= undefined;
						 ID < FirstID ->
    FirstID;
get_start_id(#mam_query{id = ID, index = undefined, direction = aft},
	     _Meta) ->
    ID + 1;
get_start_id(#mam_query{index = Index},
	     #mam_meta{first_id = FirstID}) ->
    FirstID + Index.

-spec check_request(mam_query()) -> ok | {error, _}.

check_request(#mam_query{id = error}) ->
    ?DEBUG("Got invalid <before/> or <after/> value", []),
    {error, ?ERR_BAD_REQUEST};
check_request(#mam_query{index = error}) ->
    ?DEBUG("Got invalid <index/> value", []),
    {error, ?ERR_BAD_REQUEST};
check_request(#mam_query{max = error}) ->
    ?DEBUG("Got invalid <max/> value", []),
    {error, ?ERR_BAD_REQUEST};
check_request(#mam_query{filter = #mam_filter{start = error}}) ->
    ?DEBUG("Got invalid <start/> value", []),
    {error, ?ERR_BAD_REQUEST};
check_request(#mam_query{filter = #mam_filter{fin = error}}) ->
    ?DEBUG("Got invalid <end/> value", []),
    {error, ?ERR_BAD_REQUEST};
check_request(#mam_query{filter = #mam_filter{with = error}}) ->
    ?DEBUG("Got invalid <with/> value", []),
    {error, ?ERR_BAD_REQUEST};
check_request(#mam_query{index = Index, filter = Filter})
    when is_integer(Index), Filter =/= #mam_filter{} ->
    %% We don't support both an index and filters.
    ?DEBUG("Cannot use <index/> with filters", []),
    {error, ?ERR_FEATURE_NOT_IMPLEMENTED};
check_request(_Query) ->
    ok.

%%--------------------------------------------------------------------
%% Send responses.
%%--------------------------------------------------------------------

-spec send_iq_result(jid(), iq_request()) -> ok.

send_iq_result(#jid{luser = U, lserver = S} = JID, IQ) ->
    ?DEBUG("Sending IQ result to ~s", [jlib:jid_to_string(JID)]),
    Response = jlib:make_result_iq_reply(jlib:iq_to_xml(IQ#iq{sub_el = []})),
    ejabberd_router:route(jlib:make_jid(U, S, <<"">>), JID, Response).

-spec send_mam_messages(jid(), mam_query_id() | undefined, [mam_msg()]) -> ok.

send_mam_messages(JID, QueryID, Msgs) ->
    lists:foreach(fun(Msg) -> send_mam_message(JID, QueryID, Msg) end, Msgs).

-spec send_mam_message(jid(), mam_query_id() | undefined, mam_msg()) -> ok.

send_mam_message(#jid{luser = U, lserver = S} = JID, QueryID,
		 #mam_msg{key = {_US, MamID}, stanza = Stanza, time = Time}) ->
    ID = jlib:encode_base64(crypto:rand_bytes(9)),
    To = jlib:jid_to_string(JID),
    QueryIDAttr = if is_binary(QueryID) ->
			  [{<<"queryid">>, QueryID}];
		     QueryID =:= undefined ->
			  []
		  end,
    NoCopy = #xmlel{name = <<"no-copy">>,
		    attrs = [{<<"xmlns">>, ?NS_HINTS}]},
    Forwarded = #xmlel{name = <<"forwarded">>,
		       attrs = [{<<"xmlns">>, ?NS_FORWARD}],
		       children = [xml:replace_tag_attr(<<"xmlns">>,
							<<"jabber:client">>,
							Stanza)]},
    Result = #xmlel{name = <<"result">>,
		    attrs = [{<<"xmlns">>, ?NS_MAM},
			     {<<"id">>, jlib:integer_to_binary(MamID)}]
			     ++ QueryIDAttr,
		    children = [jlib:add_delay_info(Forwarded, S, Time)]},
    Message = #xmlel{name = <<"message">>,
		     attrs = [{<<"id">>, ID}, {<<"to">>, To}],
		     children = [Result, NoCopy]},
    ?DEBUG("Sending MAM message ~B to ~s", [MamID, To]),
    ejabberd_router:route(jlib:make_jid(U, S, <<"">>), JID, Message).

-spec send_fin_message(jid(), mam_query_id(), mam_result()) -> ok.

send_fin_message(#jid{luser = U, lserver = S} = JID, QueryID,
		 #mam_result{count = Count,
			     index = Index,
			     first = First,
			     last = Last,
			     is_complete = IsComplete}) ->
    ID = jlib:encode_base64(crypto:rand_bytes(9)),
    To = jlib:jid_to_string(JID),
    QueryIDAttr = if is_binary(QueryID) ->
			  [{<<"queryid">>, QueryID}];
		     QueryID =:= undefined ->
			  []
		  end,
    NoCopy = #xmlel{name = <<"no-copy">>, attrs = [{<<"xmlns">>, ?NS_HINTS}]},
    CompleteAttr = if IsComplete ->
			   [{<<"complete">>, <<"true">>}];
		      not IsComplete ->
			   []
		   end,
    FirstBin = if is_integer(First) ->
		       jlib:integer_to_binary(First);
		  First =:= undefined ->
		       undefined
	       end,
    LastBin = if is_integer(Last) ->
		      jlib:integer_to_binary(Last);
		 Last =:= undefined ->
		      undefined
	      end,
    RSM = #rsm_out{count = Count,
		   index = Index,
		   first = FirstBin,
		   last = LastBin},
    Fin = #xmlel{name = <<"fin">>,
		 attrs = [{<<"xmlns">>, ?NS_MAM}]
			  ++ QueryIDAttr ++ CompleteAttr,
		 children = jlib:rsm_encode(RSM)},
    Message = #xmlel{name = <<"message">>,
		     attrs = [{<<"id">>, ID}, {<<"to">>, To}],
		     children = [Fin, NoCopy]},
    ?DEBUG("Sending MAM result to ~s: ~p (~w)", [To, RSM, IsComplete]),
    ejabberd_router:route(jlib:make_jid(U, S, <<"">>), JID, Message).

%%--------------------------------------------------------------------
%% Query MAM archive.
%%--------------------------------------------------------------------

-spec query_archive(mam_query()) -> mam_result() | {error, _}.

query_archive(#mam_query{mam_jid = {U, S}, max = Max} = Query) ->
    case check_request(Query) of
      ok ->
	  DBType = gen_mod:db_type(S, ?MODULE),
	  Meta = read_meta({U, S}, DBType),
	  StartID = get_start_id(Query, Meta),
	  query_archive(Query, #mam_query_state{current = StartID,
						n_remaining = Max},
			Meta, DBType);
      {error, Error} ->
	  {error, Error}
    end.

-spec query_archive(mam_query(), mam_query_state(), mam_meta(), db_type())
      -> mam_result().

query_archive(Query,
	      #mam_query_state{n_remaining = ToDo, current = ID} = QueryState,
	      #mam_meta{first_id = FirstID, last_id = LastID} = Meta, DBType)
    when ToDo =:= 0;
	 ID =:= undefined;
	 ID < FirstID;
	 ID > LastID -> % We're done!
    #mam_result{messages = resulting_messages(Query, QueryState),
		count = resulting_count(Query, Meta),
		index = resulting_index(Query, QueryState, Meta),
		first = resulting_first(Query, QueryState),
		last = resulting_last(Query, QueryState),
		is_complete = result_is_complete(Query, QueryState, Meta,
						 DBType)};
query_archive(#mam_query{mam_jid = {U, S},
			 direction = Direction,
			 filter = Filter} = Query,
	      #mam_query_state{messages = Msgs,
			       current = ID,
			       n_remaining = N} = QueryState,
	      Meta, DBType) ->
    case read_message({{U, S}, ID}, Filter, Direction, DBType) of
      #mam_msg{} = Msg ->
	  NewQueryState =
	      case QueryState of
		#mam_query_state{first = undefined} ->
		    QueryState#mam_query_state{first = ID,
					       last = ID,
					       messages = [Msg]};
		#mam_query_state{} ->
		    QueryState#mam_query_state{last = ID,
					       messages = [Msg | Msgs]}
	      end,
	  query_next(Query, NewQueryState, Meta, N - 1, DBType);
      drop ->
	  query_next(Query, QueryState, Meta, N, DBType);
      stop ->
	  query_next(Query, QueryState, Meta, 0, DBType);
      not_found ->
	  ?DEBUG("MAM message ~B of ~s@~s not found", [ID, U, S]),
	  query_next(Query, QueryState, Meta, N - 1, DBType)
    end.

-spec query_next(mam_query(), mam_query_state(), mam_meta(),
		   non_neg_integer(), db_type()) -> mam_result().

query_next(#mam_query{direction = before} = Query,
	   #mam_query_state{current = ID} = QueryState, Meta, N, DBType) ->
    query_archive(Query, QueryState#mam_query_state{current = ID - 1,
						    n_remaining = N},
		  Meta, DBType);
query_next(#mam_query{direction = aft} = Query,
	   #mam_query_state{current = ID} = QueryState, Meta, N, DBType) ->
    query_archive(Query, QueryState#mam_query_state{current = ID + 1,
						    n_remaining = N},
		  Meta, DBType).

-spec read_meta(mam_jid(), db_type()) -> mam_meta().

read_meta(US, mnesia) ->
    case mnesia:dirty_read(mam_meta, US) of
      [M] ->
	  M;
      [] ->
	  M = #mam_meta{mam_jid = US, mam_type = user},
	  mnesia:dirty_write(M), % Initialize MAM for this user.
	  M
    end.

-spec read_message(mam_msg_key(), mam_filter(), direction(), db_type())
      -> mam_msg() | drop | stop | not_found.

read_message(Key, Filter, Direction, mnesia) ->
    ReadMsg = fun() -> mnesia:read(mam_msg, Key) end,
    case mnesia:activity(sync_dirty, ReadMsg, [], mnesia_frag) of
      [#mam_msg{} = Msg] ->
	  case filter_message(Msg, Filter, Direction) of
	    pass ->
		?DEBUG("Message ~p passes filter", [Msg]),
		Msg;
	    DropOrStop ->
		?DEBUG("Message ~p filtered: ~s", [Msg, DropOrStop]),
		DropOrStop
	  end;
      [] -> not_found
    end.

-spec filter_message(mam_msg(), mam_filter(), direction())
      -> pass | drop | stop.

filter_message(_Msg, Filter, _Direction) when Filter =:= #mam_filter{} -> pass;
filter_message(Msg, Filter, Direction) ->
    lists:foldl(fun(FilterType, pass) ->
			filter_message(FilterType, Msg, Filter, Direction);
		   (FilterType, drop) ->
			case filter_message(FilterType, Msg, Filter, Direction)
			    of
			  pass ->
			    drop;
			  DropOrStop ->
			    DropOrStop
			end;
		   (_FilterType, stop) ->
			stop
		end, pass, [start, fin, with]).

-spec filter_message(mam_filter_type(), mam_msg(), mam_filter(), direction())
      -> pass | drop | stop.

filter_message(start, _Msg, #mam_filter{start = undefined}, _Direction) ->
    pass;
filter_message(start, #mam_msg{time = Time}, #mam_filter{start = Start},
	       _Direction) when Time >= Start ->
    pass;
filter_message(start, _Msg, _Filter, before) ->
    stop;
filter_message(start, _Msg, _Filter, aft) ->
    drop;

filter_message(fin, _Msg, #mam_filter{fin = undefined}, _Direction) ->
    pass;
filter_message(fin, #mam_msg{time = Time}, #mam_filter{fin = End},
	       _Direction) when Time =< End ->
    pass;
filter_message(fin, _Msg, _Filter, aft) ->
    stop;
filter_message(fin, _Msg, _Filter, before) ->
    drop;

filter_message(with, _Msg, #mam_filter{with = undefined}, _Direction) ->
    pass;
filter_message(with, Msg, #mam_filter{with = {_U, _S, <<"">>}} = Filter,
	       _Direction) ->
    filter_message_with(bare, Msg, Filter);
filter_message(with, Msg, Filter, _Direction) ->
    filter_message_with(full, Msg, Filter).

-spec filter_message_with(bare | full, mam_msg(), mam_filter()) -> pass | drop.

filter_message_with(bare,
		    #mam_msg{from = {U, S, _}, to = {U, S, _}},
		    #mam_filter{with = {U, S, _}}) ->
    %% XEP-0313 (0.3) says: "If the 'with' field's value is the bare JID of the
    %% archive, the server must only return results where both the 'to' and
    %% 'from' match the bare JID".
    pass;
filter_message_with(bare,
		    #mam_msg{key = {{U, S}, _}},
		    #mam_filter{with = {U, S, _}}) ->
    drop;

filter_message_with(bare,
		    #mam_msg{from = {U, S, _}},
		    #mam_filter{with = {U, S, _}}) ->
    pass;
filter_message_with(bare,
		    #mam_msg{to = {U, S, _}},
		    #mam_filter{with = {U, S, _}}) ->
    pass;
filter_message_with(bare, _Msg, _Filter) ->
    drop;

filter_message_with(full,
		    #mam_msg{from = {U, S, R}},
		    #mam_filter{with = {U, S, R}}) ->
    pass;
filter_message_with(full,
		    #mam_msg{to = {U, S, R}},
		    #mam_filter{with = {U, S, R}}) ->
    pass;
filter_message_with(full, _Msg, _Filter) ->
    drop.

-spec another_message_exists(mam_query(), mam_msg_id(), db_type()) -> boolean().

another_message_exists(#mam_query{mam_jid = {U, S},
				  direction = Direction,
				  filter = Filter} = Query, ID, DBType) ->
    case read_message({{U, S}, ID}, Filter, Direction, DBType) of
      #mam_msg{} ->
	  ?DEBUG("Found another message for ~s@~s: ~B", [U, S, ID]),
	  true;
      not_found ->
	  ?DEBUG("Found no other message for ~s@~s: ~B", [U, S, ID]),
	  false;
      stop ->
	  ?DEBUG("Found no other unfiltered message for ~s@~s: ~B", [U, S, ID]),
	  false;
      drop ->
	  NextID = case Direction of
		     before ->
			 ID - 1;
		     aft ->
			 ID + 1
		   end,
	  another_message_exists(Query, NextID, DBType)
    end.

%%--------------------------------------------------------------------
%% Extract query_archive/4 results.
%%--------------------------------------------------------------------

-spec resulting_messages(mam_query(), mam_query_state()) -> [mam_msg()].

resulting_messages(#mam_query{direction = before},
		   #mam_query_state{messages = Msgs}) ->
    Msgs;
resulting_messages(#mam_query{direction = aft},
		   #mam_query_state{messages = Msgs}) ->
    lists:reverse(Msgs).

-spec resulting_count(mam_query(), mam_meta()) -> non_neg_integer() | undefined.

resulting_count(#mam_query{filter = Filter},
		#mam_meta{first_id = FirstID, last_id = LastID})
    when Filter =:= #mam_filter{},
	 is_integer(FirstID),
	 is_integer(LastID) ->
    LastID - FirstID + 1;
resulting_count(_Query, _Meta) -> undefined.

-spec resulting_index(mam_query(), mam_query_state(), mam_meta())
      -> non_neg_integer() | undefined.

resulting_index(#mam_query{filter = Filter, direction = before},
		#mam_query_state{last = Last},
		#mam_meta{first_id = FirstID})
    when Filter =:= #mam_filter{},
	 is_integer(Last),
	 is_integer(FirstID) ->
    Last - FirstID;
resulting_index(#mam_query{filter = Filter, direction = aft},
		#mam_query_state{first = First},
		#mam_meta{first_id = FirstID})
    when Filter =:= #mam_filter{},
	 is_integer(First),
	 is_integer(FirstID) ->
    First - FirstID;
resulting_index(_Query, _QueryState, _Meta) -> undefined.

-spec resulting_first(mam_query(), mam_query_state())
      -> mam_msg_id() | undefined.

resulting_first(#mam_query{direction = before},
		#mam_query_state{last = Last}) ->
    Last;
resulting_first(#mam_query{direction = aft},
		#mam_query_state{first = First}) ->
    First.

-spec resulting_last(mam_query(), mam_query_state())
      -> mam_msg_id() | undefined.

resulting_last(#mam_query{direction = before},
	       #mam_query_state{first = First}) ->
    First;
resulting_last(#mam_query{direction = aft},
	       #mam_query_state{last = Last}) ->
    Last.

-spec result_is_complete(mam_query(), mam_query_state(), mam_meta(), db_type())
      -> boolean().

result_is_complete(#mam_query{filter = Filter} = Query,
		   #mam_query_state{current = ID},
		   _Meta, DBType)
    when Filter =/= #mam_filter{}, ID =/= undefined ->
    not another_message_exists(Query, ID, DBType);
result_is_complete(#mam_query{direction = before},
		   #mam_query_state{first = First},
		   #mam_meta{first_id = FirstID}, _DBType) ->
    First =:= FirstID;
result_is_complete(#mam_query{direction = aft},
		   #mam_query_state{last = Last},
		   #mam_meta{last_id = LastID}, _DBType) ->
    Last =:= LastID.

%%--------------------------------------------------------------------
%% Remove user.
%%--------------------------------------------------------------------

-spec remove_user(binary(), binary()) -> ok.

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    DBType = gen_mod:db_type(LServer, ?MODULE),
    ?INFO_MSG("Removing MAM archive of ~s@~s", [LUser, LServer]),
    remove_user(LUser, LServer, DBType).

-spec remove_user(binary(), binary(), db_type()) -> ok.

remove_user(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    Remove =
	fun() ->
		case mnesia:read(mam_meta, US, write) of
		  [#mam_meta{first_id = undefined, last_id = undefined}] ->
		      mnesia:delete({mam_meta, US});
		  [#mam_meta{first_id = FirstID, last_id = LastID}] ->
		      DeleteMsgs =
			  fun() ->
				  DeleteMsg =
				      fun(ID) ->
					      mnesia:delete({mam_msg, {US, ID}})
				      end,
				  lists:foreach(DeleteMsg, lists:seq(FirstID,
								     LastID))
			  end,
		      mnesia:activity(transaction, DeleteMsgs, [], mnesia_frag),
		      mnesia:delete({mam_meta, US});
		  [] ->
		      ok
		end
	end,
    {atomic, ok} = mnesia:sync_transaction(Remove),
    manage_mnesia_fragments(true).
