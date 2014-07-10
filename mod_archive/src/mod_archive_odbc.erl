%%%----------------------------------------------------------------------
%%% File    : mod_archive_odbc.erl
%%% Author  : Olivier Goffart <ogoffar@kde.org> (origial mnesia version),
%%%           Alexey Shchepin <alexey@process-one.net> (PostgreSQL version),
%%%           Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua> (ODBC version)
%%% Purpose : Message Archiving using SQL DB (JEP-0136)
%%% Created : 19 Aug 2006 by Olivier Goffart <ogoffar@kde.org>
%%% Version : 1.0.1
%%% Id      : $Id$
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------

%% Options:
%%
%% default_auto_save -> true | false - is auto-save turned on by default or not;
%%     if true, default 'save' attribute will be set to 'body'.
%%
%% enforce_default_auto_save -> true | false - is auto-save default mode
%%     enforced or not; if true, requests to change it are discarded.
%%
%%  default_expire -> default time in seconds before collections are wiped out -
%%      or infinity atom.
%%
%%  enforce_min_expire -> minimal time in seconds before collections are wiped out
%%      that the user is allowed to set - or infinity atom.
%%
%%  enforce_max_expire -> maximal time in seconds before collections are wiped out
%%      that the user is allowed to set - or infinity atom.
%%
%%  replication_expire -> time in seconds before 'removed' replication
%%      information if wiped out or infinity atom to disable.
%%
%%  session_duration -> time in secondes before the timeout of a session.
%%
%%  wipeout_interval -> time in seconds between wipeout runs or infinity atom
%%                      to disable.
%%
%%
%% Please note that according to XEP-136 only the following auto_save
%% combinations are valid:
%%
%% 1) default_auto_save = true, enforce_default_auto_save = true
%% 2) default_auto_save = false, enforce_default_auto_save = false
%%
%% Implementation will happily work with any combination of these,
%% though - for example, for personal ejabberd server, until all clients
%% support XEP-136, combination default_auto_save = true,
%% enforce_default_auto_save = false is quite logical, while for some
%% public ejabberd server with lots of users and shortage of disk space
%% default_auto_save = false, enforce_default_auto_save = true might be
%% desirable.
%%
%% Default values:
%% - default_auto_save = false
%% - enforce_default_auto_save = false
%% - default_expire = infinity
%% - enforce_min_expire = 0
%% - enforce_max_expire = infinity
%% - replication_expire = 31536000 (= 1 year)
%% - session_duration = 1800
%% - wipeout_interval = 86400 (= 1 day)


-module(mod_archive_odbc).
-author('ogoffart@kde.org').
-author('alexey@process-one.net').
-author('ejabberd@ndl.kiev.ua').

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start_link/2, start/2, stop/1,
         remove_user/2,
         send_packet/3,
         receive_packet/3,
         receive_packet/4,
         process_iq/3, process_local_iq/3,
         get_disco_features/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-record(state, {host,
                sessions,
                session_duration}).

-define(PROCNAME, ejabberd_mod_archive_odbc).
-define(NS_ARCHIVE,
        "http://www.xmpp.org/extensions/xep-0136.html#ns").
-define(NS_ARCHIVE_AUTO,
        "http://www.xmpp.org/extensions/xep-0136.html#ns-auto").
-define(NS_ARCHIVE_MANAGE,
        "http://www.xmpp.org/extensions/xep-0136.html#ns-manage").
-define(NS_ARCHIVE_PREF,
        "http://www.xmpp.org/extensions/xep-0136.html#ns-pref").
-define(NS_ARCHIVE_MANUAL,
        "http://www.xmpp.org/extensions/xep-0136.html#ns-manual").
-define(INFINITY, calendar:datetime_to_gregorian_seconds({{2038,1,19},{0,0,0}})).

%% Should be OK for most of modern DBs, I hope ...
-define(MAX_QUERY_LENGTH, 32768).

-define(MYDEBUG(Format, Args),
        io:format("D(~p:~p:~p) : " ++ Format ++ "~n",
                  [calendar:local_time(), ?MODULE, ?LINE] ++ Args)).

-record(archive_jid_prefs,
        {us,
         jid,
         save = undefined,
         expire = undefined,
         otr = undefined}).

-record(archive_global_prefs,
        {us,
         save = undefined,
         expire = undefined,
         otr = undefined,
         method_auto = undefined,
         method_local = undefined,
         method_manual = undefined,
         auto_save = undefined}).

-record(archive_collection,
        {id,
         us,
         jid,
         utc,
         change_by,
         change_utc,
         deleted,
         subject = "",
         prev = [],
         next = [],
         thread = "",
         crypt = false,
         extra = ""}).

-record(archive_message,
        {id,
         coll_id,
         utc,
         direction,
         body,
         name = ""}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
        {Proc,
         {?MODULE, start_link, [Host, Opts]},
         permanent,
         1000,
         worker,
         [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).
%% ejabberd-1.x compatibility code
%% NOTE: keepalive is not supported in ejabberd 1.x, so
%% you'll either need to turn off connections timeout in DB
%% configuration or invent smth else ...
%%    ChildSpecODBC =
%%         {gen_mod:get_module_proc(Host, ejabberd_odbc_sup),
%%         {ejabberd_odbc_sup, start_link, [Host]},
%%         permanent,
%%         infinity,
%%         supervisor,
%%         [ejabberd_odbc_sup]},
%%    supervisor:start_child(ejabberd_sup, ChildSpecODBC).
%% EOF ejabberd-1.x compatibility code

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc).
%% ejabberd-1.x compatibility code
%%    ProcODBC = gen_mod:get_module_proc(Host, ejabberd_odbc_sup),
%%    gen_server:call(ProcODBC, stop),
%%    supervisor:delete_child(ejabberd_sup, ProcODBC).
%% EOF ejabberd-1.x compatibility code

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    SessionDuration = gen_mod:get_opt(session_duration, Opts, 1800),
    WipeOutInterval = gen_mod:get_opt(wipeout_interval, Opts, 86400),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ARCHIVE, ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_ARCHIVE, ?MODULE, process_local_iq, IQDisc),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, send_packet, 90),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, receive_packet, 90),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, receive_packet, 35),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, get_disco_features, 99),
    timer:send_interval(1000 * SessionDuration div 2, clean_sessions),
    case WipeOutInterval of
        infinity -> [];
        N when is_integer(N) -> timer:send_interval(1000 * N, wipeout_collections)
    end,
    {ok, #state{host = Host,
                sessions = dict:new(),
                session_duration = SessionDuration}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({addlog, Type, Direction, LUser, LServer, LResource, JID, Thread, Subject, Nick, Body}, State) ->
    Sessions = State#state.sessions,
    NewSessions =
        case should_store_jid({LUser, LServer}, JID) of
            false ->
                Sessions;
            true when Type == "groupchat", Direction == to ->
                Sessions;
            true ->
                do_log(Sessions, LUser, LServer, LResource, JID,
                       Type, Direction, Thread, Subject, Nick, Body,
                       State#state.session_duration)
        end,
    {noreply, State#state{sessions = NewSessions}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(clean_sessions, State) ->
    Sessions = State#state.sessions,
    Timeout = State#state.session_duration,
    TS = get_timestamp(),
    F = fun(_, Value)->
		dict:filter(fun(_, {_Start, Last, _, _}) ->
				    TS - Last =< Timeout
			    end, Value)
	end,
    FilteredSessions = dict:map(F, Sessions),
    NewSessions = dict:filter(fun(_Key, Value) ->
				      dict:fetch_keys(Value) /= []
                              end, FilteredSessions),
    {noreply, State#state{sessions = NewSessions}};

handle_info(wipeout_collections, State) ->
    expire_collections(State#state.host),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_ARCHIVE),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ARCHIVE),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, send_packet, 90),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, receive_packet, 90),
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, receive_packet, 35),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, get_disco_features, 99),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Workaround the fact that if the client send <iq type='get'>
%% it end up like <iq type='get' from='u@h' to = 'u@h'>
process_iq(From, To, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    #jid{lserver = LServer, luser = LUser} = To,
    #jid{luser = FromUser} = From,
    case {LUser, LServer, lists:member(LServer, ?MYHOSTS)} of
        {FromUser, _, true} ->
            process_local_iq(From, To, IQ);
        {"", _, true} ->
            process_local_iq(From, To, IQ);
        {"", "", _} ->
            process_local_iq(From, To, IQ);
        _ ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.

process_local_iq(From, To, #iq{sub_el = SubEl} = IQ) ->
    case lists:member(From#jid.lserver, ?MYHOSTS) of
        false ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
        true ->
            {xmlelement, Name, _Attrs, _Els} = SubEl,
            F = fun() ->
			case Name of
			    "pref" -> process_local_iq_pref(From, To, IQ);
			    "auto" -> process_local_iq_auto(From, To, IQ);
			    "list" -> process_local_iq_list(From, To, IQ);
			    "retrieve" -> process_local_iq_retrieve(From, To, IQ);
			    "save" -> process_local_iq_save(From, To, IQ);
			    "remove" -> process_local_iq_remove(From, To, IQ);
			    "modified" -> process_local_iq_modified(From, To, IQ);
			    _ -> IQ#iq{type = error,
				       sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
			end
		end,
	    %% All IQ processing functions should return either {result, xmlelement} or
	    %% {error, xmlelement} - other returns mean smth is seriously wrong
	    %% with the code itself.
            case catch F() of
                {result, R} ->
                    IQ#iq{type = result, sub_el = R};
                {error, Err} ->
                    IQ#iq{type = error,
                          sub_el = [SubEl, Err]};
                {'EXIT', Ex} ->
                    ?ERROR_MSG("catched unhandled exception: ~p", [Ex]),
                    IQ#iq{type = error,
                          sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
                Res ->
                    ?ERROR_MSG("unexpected result: ~p", [Res]),
                    IQ#iq{type = error,
                          sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
            end
    end.


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    SUS = get_us_escaped(US),
    F = fun() ->
                run_sql_query(
		  ["delete from archive_jid_prefs "
		   "where us = ", SUS]),
                run_sql_query(
		  ["delete from archive_global_prefs "
		   "where us = ", SUS]),
                run_sql_query(
		  ["delete from archive_collections "
		   "where us = ", SUS])
        end,
    run_sql_transaction(LServer, F).

get_disco_features(Acc, _From, _To, "", _Lang) ->
    Features =
        case Acc of
            {result, I} -> I;
            _ -> []
        end,
    {result, Features ++ [?NS_ARCHIVE_MANAGE,
                          ?NS_ARCHIVE_AUTO,
                          ?NS_ARCHIVE_PREF,
                          ?NS_ARCHIVE_MANUAL]};

get_disco_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  3 Automated archiving
%%

send_packet(From, To, Packet) ->
    add_log(to, From#jid.luser, From#jid.lserver, From#jid.lresource, To, Packet).

receive_packet(From, To, Packet) ->
    add_log(from, To#jid.luser, To#jid.lserver, To#jid.lresource, From, Packet).

receive_packet(_JID, From, To, Packet) ->
    receive_packet(From, To, Packet).

add_log(Direction, LUser, LServer, LResource, JID, Packet) ->
    case parse_message(Packet) of
        {Type, Thread, Subject, Nick, Body}  ->
            Proc = gen_mod:get_module_proc(LServer, ?PROCNAME),
            gen_server:cast(
              Proc, {addlog, Type, Direction, LUser, LServer, LResource, JID, Thread, Subject, Nick, Body});
	_ ->
            ok
    end.

%% Parse the message and return {Thread, Subject, Body} strings if successful
parse_message({xmlelement, "message", _, _} = Packet) ->
    case xml:get_tag_attr_s("type", Packet) of
        Type when Type == "";
                  Type == "normal";
                  Type == "chat";
                  Type == "groupchat" ->
            case xml:get_subtag(Packet, "body") of
                false ->
		    "";
                _ ->
                    {Type,
                     xml:get_path_s(Packet, [{elem, "thread"}, cdata]),
                     xml:get_path_s(Packet, [{elem, "subject"}, cdata]),
                     xml:get_path_s(Packet, [{elem, "nick"}, cdata]),
                     xml:get_path_s(Packet, [{elem, "body"}, cdata])}
            end;
        _ ->
            ""
    end;
parse_message(_) ->
    "".

%% archive the message Body    return new Sessions
%%  Sessions:  a dict of open sessions
%%  LUser, LServer :  the local user's information
%%  Jid : the contact's jid
%%  Body : the message body
do_log(Sessions, LUser, LServer, LResource, JID, Type, Direction, Thread, Subject,
       Nick, Body, SessionDuration) ->
    LowJID = jlib:jid_tolower(JID),
    {_, _, Resource} = LowJID,
    F = fun() ->
                {NewSessions, _Start, TS, CID, NewRes} =
		    find_storage(LUser, LServer, LowJID, Thread, Sessions,
				 SessionDuration, Type),
                LJID = jlib:jid_tolower(jlib:make_jid(LUser, LServer, LResource)),
                update_collection_partial(CID, LServer, Thread, Subject, NewRes, LJID, TS),
                M = #archive_message{coll_id = CID,
                                     utc = TS,
                                     direction = Direction,
                                     name =
                                         if Type == "groupchat" ->
                                             if Nick /= "" ->
					         Nick;
                                                true ->
                                                 Resource
                                                end;
                                            true -> ""
                                         end,
                                     body = Body},
                store_message(LServer, M),
                NewSessions
        end,
    case run_sql_transaction(LServer, F) of
        {error, Err} ->
            ?ERROR_MSG("error when performing automated archiving: ~p", [Err]),
            Sessions;
        R -> %?MYDEBUG("successfull automated archiving: ~p", [R]),
	    R
    end.

find_storage(LUser, LServer, JID, Thread, Sessions, Timeout, Type) ->
    %%
    %% In fact there's small problem with resources: we can send the message
    %% to recepient without specifying resource (typically, when sending the first
    %% message), or with it (for subsequent ones). On the other hand,
    %% remote sender will always (?) use resource when sending us the message.
    %%
    %% This means that we either should strip resouce completely from JID when
    %% creating the key (which is easy, but not nice, as then all messages will be
    %% put into single collection without treating resources at all), or use more
    %% intelligent approach to match the appropriate collection to our message.
    %%
    %% Additionally we'd like to use "thread" to differentiate between different
    %% conversations, to put them into different collections.
    %%
    %% Here is the approach we use:
    %%
    %% 1) There's two levels key schema: first key is JID without resource, second-level
    %%    key is the thread. If thread is not present, {no_thread, Resource} is used
    %%    instead.
    %% 2) If thread is specified in the message - just use both-levels keys normally,
    %%    reusing existing collections if there's a match or creating new one if no
    %%    matching collection found.
    %% 3) Otherwise use first-level key to get all sub-items, then
    %%      * If resource IS specified: search for matching resource:
    %%        - if found - use it.
    %%        - if not, search for sub-item with empty resource. If found, use it
    %%          and rewrite its resource to ours, notifying the caller to store collection.
    %%          If not - create new one.
    %%      * If resource IS NOT specified: use the most recent sub-item or
    %%        create new if none exists, notifying the caller about change of
    %%        resource, if needed.
    %%
    {_, _, ResourceIn} = JID,
    %% Assume empty Resource for groupchat messages so that they're recorded
    %% to the same collection.
    Resource = if Type == "groupchat" -> ""; true -> ResourceIn end,
    Key1 = {LUser, LServer, jlib:jid_remove_resource(JID)},
    TS = get_timestamp(),
    case dict:find(Key1, Sessions) of
        error ->
            new_dict_answer(Key1, TS, Thread, Resource, Sessions);
        {ok, Val1} ->
            if Thread /= "" ->
		    case dict:find(Thread, Val1) of
			error ->
			    new_dict_answer(Key1, TS, Thread, Resource, Sessions);
			{ok, Val2} ->
			    updated_dict_answer(Key1, Val2, TS, Thread,
						Resource, Sessions, Timeout)
		    end;
	       true ->
		    if Resource /= "" ->
			    case dict:find({no_thread, Resource}, Val1) of
				{ok, Val2} ->
				    updated_dict_answer(Key1, Val2, TS, Thread,
							Resource, Sessions, Timeout);
				error ->
				    case dict:find({no_thread, ""}, Val1) of
					error ->
					    new_dict_answer(Key1, TS, Thread, Resource,
							    Sessions);
					{ok, Val2} ->
					    updated_dict_answer(Key1, Val2, TS,
								Thread, Resource, Sessions, Timeout)
				    end
			    end;
		       true ->
			    F = fun(_, Value, {_, MaxLast, _, _} = OldVal) ->
					{_, Last, _, CurRes} = Value,
					if ((Type /= "groupchat") and (Last > MaxLast)) or
                                           ((Type == "groupchat") and (CurRes == "")) ->
                                                Value;
					   true -> OldVal
					end
				end,
			    case dict:fold(F, {-1, -1, null, ""}, Val1) of
				{_, -1, _, _} ->
				    new_dict_answer(Key1, TS, Thread, Resource,
						    Sessions);
				{_, _, _, Res} = Val2 ->
				    updated_dict_answer(Key1, Val2, TS, Thread,
							Res, Sessions, Timeout)
			    end
		    end
            end
    end.

updated_dict(Key1, Start, TS, CID, Thread, Resource, Sessions) ->
    Val1 = case dict:find(Key1, Sessions) of
               error -> dict:new();
               {ok, V} -> V
           end,
    Key2 = if Thread /= "" -> Thread; true -> {no_thread, Resource} end,
    Val2 = {Start, TS, CID, Resource},
    NVal1 = dict:store(Key2, Val2, Val1),
    dict:store(Key1, NVal1, Sessions).

updated_dict_answer(Key1, {Start, Last, CID, _OldRes}, TS, Thread, Resource,
                    Sessions, Timeout) ->
    if TS - Last > Timeout ->
	    new_dict_answer(Key1, TS, Thread, Resource, Sessions);
       true ->
	    {updated_dict(Key1, Start, TS, CID, Thread, Resource, Sessions),
	     Start, TS, CID, Resource}
    end.

new_dict_answer({LUser, LServer, JID} = Key1, TS, Thread, Resource, Sessions) ->
    CID = get_collection_id({LUser, LServer, JID, TS}),
    {updated_dict(Key1, TS, TS, CID, Thread, Resource, Sessions),
     TS, TS, CID, Resource}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  3.1 Preferences
%%

process_local_iq_pref(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    Result = case Type of
		 set ->
		     {xmlelement, _Name, _Attrs, Els} = SubEl,
		     process_save_set(From#jid.luser, From#jid.lserver, Els);
		 get ->
		     process_save_get(From#jid.luser, From#jid.lserver)
	     end,
    case Result of
        ok ->
            broadcast_iq(From, IQ#iq{type = set, sub_el=[SubEl]}),
            {result, []};
        R -> R
    end.

%% returns {error, xmlelement} or {result, xmlelement}
process_save_get(LUser, LServer) ->
    F =
        fun() ->
		%% Request prefs for all of JIDs
		LItems =
		    lists:map(
		      fun(J) ->
			      jid_prefs_to_xml(J)
		      end, get_all_jids_prefs({LUser, LServer})),
		GPrefs = get_global_prefs({LUser, LServer}),
		DGPrefs = default_global_prefs({LUser, LServer}),
		UnSet =
		    if (GPrefs#archive_global_prefs.save /= undefined) or
		       (GPrefs#archive_global_prefs.expire /= undefined) or
		       (GPrefs#archive_global_prefs.otr /= undefined) -> "false";
		       true -> "true"
		    end,
		DItem = global_prefs_to_xml(GPrefs, DGPrefs, UnSet),
		{result, [{xmlelement, "pref", [{"xmlns", ?NS_ARCHIVE}], DItem ++ LItems}]}
        end,
    run_sql_transaction(LServer, F).

jid_prefs_to_xml(Pref) ->
    Save = Pref#archive_jid_prefs.save,
    Expire = Pref#archive_jid_prefs.expire,
    OTR = Pref#archive_jid_prefs.otr,
    {xmlelement, "item",
     [{"jid", jlib:jid_to_string(Pref#archive_jid_prefs.jid)}] ++
     if Save /= undefined ->
	     [{"save", atom_to_list(Save)}];
	true ->
	     []
     end  ++
     if Expire /= undefined, Expire /= infinity ->
	     [{"expire", integer_to_list(Expire)}];
	true ->
	     []
     end ++
     if OTR /= undefined ->
	     [{"otr", atom_to_list(OTR)}];
	true ->
	     []
     end, []}.

global_prefs_to_xml(GPrefs, DGPrefs, UnSet) ->
    Prefs = list_to_tuple(
	      lists:zipwith(
		fun(Item1, Item2) ->
			if Item1 /= undefined -> Item1;
			   true -> Item2
			end
		end,
		tuple_to_list(GPrefs),
		tuple_to_list(DGPrefs))),
    Expire = Prefs#archive_global_prefs.expire,
    [{xmlelement, "default",
      [{"save", atom_to_list(Prefs#archive_global_prefs.save)}] ++
      if Expire /= infinity -> [{"expire", integer_to_list(Expire)}]; true -> [] end ++
      [{"otr", atom_to_list(Prefs#archive_global_prefs.otr)},
       {"unset", UnSet}],
      []},
     {xmlelement, "method",
      [{"type", "auto"},
       {"use", atom_to_list(Prefs#archive_global_prefs.method_auto)}],
      []},
     {xmlelement, "method",
      [{"type", "local"},
       {"use", atom_to_list(Prefs#archive_global_prefs.method_local)}],
      []},
     {xmlelement, "method",
      [{"type", "manual"},
       {"use", atom_to_list(Prefs#archive_global_prefs.method_manual)}],
      []},
     {xmlelement, "auto",
      [{"save", atom_to_list(Prefs#archive_global_prefs.auto_save)}],
      []}].

%% Returns the archive_global_prefs record filled with default values
default_global_prefs({_, LServer} = US) ->
    DefaultAutoSave = gen_mod:get_module_opt(LServer, ?MODULE, default_auto_save, false),
    DefaultExpire = gen_mod:get_module_opt(LServer, ?MODULE, default_expire, infinity),
    #archive_global_prefs{us = US,
                          save = if DefaultAutoSave -> body; true -> false end,
                          expire = DefaultExpire,
                          method_auto = if DefaultAutoSave -> prefer; true -> concede end,
                          method_local = concede,
                          method_manual = if DefaultAutoSave -> concede; true -> prefer end,
                          auto_save = DefaultAutoSave,
                          otr = forbid}.


%% Return {error, xmlelement} or ok
process_save_set(LUser, LServer, Elems) ->
    F =
        fun() ->
		US = {LUser, LServer},
		GPrefs = get_global_prefs(US),
		GPrefs1 = GPrefs#archive_global_prefs{us = US},
		parse_save_elem(GPrefs1, Elems),
		ok
        end,
    run_sql_transaction(LServer, F).

parse_save_elem(GPrefs, [{xmlelement, "default", Attrs, _} | Tail]) ->
    {Save, Expire, OTR} = get_main_prefs_from_attrs(Attrs),
    GPrefs1 = GPrefs#archive_global_prefs{save = Save, expire = Expire, otr = OTR},
    parse_save_elem(GPrefs1, Tail);

parse_save_elem(GPrefs, [{xmlelement, "method", Attrs, _} | Tail]) ->
    Use =
        case xml:get_attr_s("use", Attrs) of
            "concede" -> concede;
            "forbid" -> forbid;
            "prefer" -> prefer;
            "" -> undefined;
            _ -> throw({error, ?ERR_BAD_REQUEST})
        end,
    GPrefs1 =
        case xml:get_attr_s("type", Attrs) of
            "auto" -> GPrefs#archive_global_prefs{method_auto = Use};
            "local" -> GPrefs#archive_global_prefs{method_local = Use};
            "manual" -> GPrefs#archive_global_prefs{method_manual = Use};
            "" -> GPrefs;
            _ -> throw({error, ?ERR_BAD_REQUEST})
        end,
    parse_save_elem(GPrefs1, Tail);

parse_save_elem(GPrefs, [{xmlelement, "auto", Attrs, _} | Tail]) ->
    GPrefs1 =
        case xml:get_attr_s("save", Attrs) of
            "true" -> GPrefs#archive_global_prefs{auto_save = true};
            "false" -> GPrefs#archive_global_prefs{auto_save = false};
            _ -> throw({error, ?ERR_BAD_REQUEST})
        end,
    parse_save_elem(GPrefs1, Tail);

parse_save_elem(GPrefs, [{xmlelement, "item", Attrs, _}  | Tail]) ->
    case jlib:string_to_jid(xml:get_attr_s("jid", Attrs)) of
        error -> throw({error, ?ERR_JID_MALFORMED});
        JID ->
            LJID = jlib:jid_tolower(JID),
            {Save, Expire, OTR} = get_main_prefs_from_attrs(Attrs),
            Prefs = #archive_jid_prefs{us = GPrefs#archive_global_prefs.us,
                                       jid = LJID,
                                       save = Save,
                                       expire = Expire,
                                       otr = OTR},
            store_jid_prefs(Prefs)
    end,
    parse_save_elem(GPrefs, Tail);

parse_save_elem(GPrefs, []) ->
    store_global_prefs(GPrefs);

parse_save_elem(GPrefs, [_ | Tail]) ->
    parse_save_elem(GPrefs,  Tail).

get_main_prefs_from_attrs(Attrs) ->
    Save =
        case xml:get_attr_s("save", Attrs) of
            "body" -> body;
            "false" -> false;
            "" -> undefined;
            _ -> throw({error, ?ERR_BAD_REQUEST})
        end,
    Expire =
        case xml:get_attr_s("expire", Attrs) of
            "" -> undefined;
            N -> case catch list_to_integer(N) of
                     NR when is_integer(NR) -> NR;
                     _ -> throw({eror, ?ERR_BAD_REQUEST})
                 end
        end,
    OTR =
        case xml:get_attr_s("otr", Attrs) of
            "" -> undefined;
            V -> list_to_atom(V)
        end,
    {Save, Expire, OTR}.


broadcast_iq(#jid{luser = User, lserver = Server}, IQ) ->
    Fun = fun(Resource) ->
		  ejabberd_router:route(
                    jlib:make_jid("", Server, ""),
                    jlib:make_jid(User, Server, Resource),
                    jlib:iq_to_xml(IQ#iq{id="push"}))
	  end,
    lists:foreach(Fun, ejabberd_sm:get_user_resources(User,Server)).


process_local_iq_auto(From, _To, #iq{type = Type, sub_el = SubEl}) ->
    case Type of
        set ->
            {xmlelement, _Name, Attrs, _Els} = SubEl,
            Auto =
                case xml:get_attr_s("save", Attrs) of
                    "true" -> true;
                    "false" -> false;
                    _ -> throw({error, ?ERR_BAD_REQUEST})
                end,
            LUser = From#jid.luser,
            LServer = From#jid.lserver,
            F =
                fun() ->
			US = {LUser, LServer},
			GPrefs = get_global_prefs(US),
			GPrefs1 = GPrefs#archive_global_prefs{us = US, auto_save = Auto},
			store_global_prefs(GPrefs1),
			{result, []}
                end,
            run_sql_transaction(LServer, F);
        get ->
            throw({error, ?ERR_BAD_REQUEST})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility function

%% Return true if LUser@LServer should log message for the contact JID
should_store_jid({_, LServer} = US, JID) ->
    F =
        fun() ->
		GPrefs = get_global_prefs(US),
		case GPrefs#archive_global_prefs.auto_save of
		    false ->
			false;
		    true ->
			should_store_jid_full_check(US, JID);
		    undefined ->
			DGPrefs = default_global_prefs(US),
			if
			    DGPrefs#archive_global_prefs.auto_save == false ->
				false;
			    true ->
				should_store_jid_full_check(US, JID)
			end
		end
        end,
    case run_sql_transaction(LServer, F) of
        {error, Err} -> ?ERROR_MSG("should_store_jid failed: ~p", [Err]), false;
        R -> R
    end.

should_store_jid_full_check(US, JID) ->
    {User, Server, Res} = jlib:jid_tolower(JID),
    Prefs1 = get_jid_prefs(US, {User, Server, Res}),
    Save1 = Prefs1#archive_jid_prefs.save,
    Save2 =
        if Save1 == undefined ->
		Prefs2 = get_jid_prefs(US, {User, Server, ""}),
		Prefs2#archive_jid_prefs.save;
           true -> Save1
        end,
    Save3 =
        if Save2 == undefined ->
		Prefs3 = get_jid_prefs(US, {"", Server, ""}),
		Prefs3#archive_jid_prefs.save;
           true -> Save2
        end,
    case Save3 of
        false -> false;
        _ -> true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   4. Manual Archiving
%%


process_local_iq_save(From, _To, #iq{type = Type, sub_el = SubEl}) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = From,
    case Type of
        get ->
            throw({error, ?ERR_NOT_ALLOWED});
        set ->
            {C, Msgs} = parse_store_element(LUser, LServer, SubEl),
            F =
                fun() ->
			CID = get_collection_id({LUser, LServer,
						 C#archive_collection.jid,
						 C#archive_collection.utc}),
			C1 = get_collection_by_id(CID),
			LJID = jlib:jid_tolower(jlib:make_jid(LUser, LServer, LResource)),
			C2 = C#archive_collection{id = CID,
						  change_by = LJID,
						  change_utc = get_timestamp()},
			C3 =
			    list_to_tuple(
			      lists:zipwith(
                                fun(ValOld, ValNew) ->
					case ValNew of
					    undefined -> ValOld;
					    _ -> ValNew
					end
                                end,
                                tuple_to_list(C1), tuple_to_list(C2))),
			store_collection(C3),
			store_messages(LServer, CID, Msgs),
			{result, []}
                end,
            run_sql_transaction(LServer, F)
    end.

%% return a {#archive_collection, list of #archive_message} or {error, xmlelement}
parse_store_element(LUser, LServer,
                    {xmlelement, "save", _ChatAttrs, ChatSubEls}) ->
    case xml:remove_cdata(ChatSubEls) of
        [{xmlelement, "chat", Attrs, SubEls} = SubEl] ->
            {LUser, LServer, Jid, Start} = link_from_argument(LUser, LServer, SubEl),
            Extra = xml:get_subtag(SubEl, "x"),
            C = #archive_collection{us = {LUser, LServer},
                                    jid = Jid,
                                    utc = Start,
                                    prev = get_link_as_list(SubEls, "previous"),
                                    next = get_link_as_list(SubEls, "next"),
                                    subject = case xml:get_attr("subject", Attrs) of
                                                  {value, Val} -> Val;
                                                  false -> undefined
                                              end,
                                    thread = case xml:get_attr("thread", Attrs) of
						 {value, Val} -> Val;
						 false -> undefined
                                             end,
                                    extra =
				    if Extra /= false -> encode_extra(Extra);
				       true -> undefined
				    end},
            Messages = parse_store_element_sub(SubEls, Start),
            {C, Messages};
        _ ->
            throw({error, ?ERR_BAD_REQUEST})
    end.

parse_store_element_sub([{xmlelement, Dir, _, _}  = E | Tail], Start)
  when Dir == "from";
       Dir == "to";
       Dir == "note" ->
    UTC =
	case xml:get_tag_attr_s("secs", E) of
	    "" ->
		case xml:get_tag_attr_s("utc", E) of
		    "" -> throw({error, ?ERR_BAD_REQUEST});
		    Val -> get_seconds_from_datetime_string(Val)
		end;
	    Secs ->
		Start +
		    case list_to_integer(Secs) of
			N when is_integer(N) -> N;
			_ -> throw({error, ?ERR_BAD_REQUEST})
		    end
	end,
    Body = if Dir == "note" -> xml:get_tag_cdata(E);
              true -> xml:get_tag_cdata(xml:get_subtag(E,"body"))
           end,
    [#archive_message{direction = list_to_atom(Dir),
		      utc = UTC,
		      body = Body,
		      name = xml:get_tag_attr_s("name", E)} |
     parse_store_element_sub(Tail, Start)];

parse_store_element_sub([], _) -> [];

parse_store_element_sub([_ | Tail], Start) -> parse_store_element_sub(Tail, Start).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   5. Archive Management
%%


process_local_iq_list(From, _To, #iq{type = Type, sub_el = SubEl}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case Type of
        set ->
            throw({error, ?ERR_NOT_ALLOWED});
        get ->
            {xmlelement, _, _, SubEls} = SubEl,
            RSM = parse_rsm(SubEls),
            F = fun() ->
			{interval, Start, Stop, JID} = parse_root_argument(SubEl),
			Req = get_combined_req(Start, Stop, RSM),
			{ok, Items, RSM_Elem} = get_collections_links(LUser, LServer, Req, JID),
			{result, [{xmlelement, "list",
				   [{"xmlns", ?NS_ARCHIVE}],
				   lists:append(
				     lists:map(
                                       fun(C) ->
					       collection_link_to_xml("chat", C)
                                       end, Items),
				     RSM_Elem)}]}
                end,
            run_sql_transaction(LServer, F)
    end.


process_local_iq_retrieve(From, _To, #iq{type = Type, sub_el = SubEl}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case Type of
        set ->
            throw({error, ?ERR_NOT_ALLOWED});
        get ->
            {xmlelement, _, _, SubEls} = SubEl,
            RSM = parse_rsm(SubEls),
            F = fun() ->
			Link = link_from_argument(LUser, LServer, SubEl),
			Store = retrieve_collection_and_msgs(Link, RSM),
			{result, Store}
                end,
            run_sql_transaction(LServer, F)
    end.


retrieve_collection_and_msgs(Link, RSM) ->
    C = get_collection(Link),
    {ok, Items, RSM_Elem} = get_messages(C, RSM),
    {_, _, Attrs, SubEls} = collection_to_xml(C),
    [{xmlelement, "chat", Attrs,
      lists:append([SubEls,
                    lists:map(
		      fun(M) ->
			      message_to_xml(M, C#archive_collection.utc)
		      end, Items),
                    RSM_Elem])}].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   5.3 Removing a Collection
%%


process_local_iq_remove(From, _To, #iq{type = Type, sub_el = SubEl}) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = From,
    case Type of
        get ->
            throw({error, ?ERR_NOT_ALLOWED});
        set ->
            {xmlelement, _,  _, _} = SubEl,
            {interval, Start, Stop, Jid} = parse_root_argument(SubEl),
            process_remove_interval(LUser, LServer, LResource, Start, Stop, Jid)
    end.

process_remove_interval(LUser, LServer, LResource, Start, End, With) ->
    SUS = get_us_escaped({LUser, LServer}),
    WithCond = case With of
                   undefined ->
                       "";
                   JID ->
                       {SUser, SServer, SResource} = get_jid_escaped(JID),
                       SServerNonEmpty = is_non_empty(SServer),
                       SUserNonEmpty = is_non_empty(SUser),
                       SResourceNonEmpty = is_non_empty(SResource),
                       [if SServerNonEmpty == true -> ["with_server = ", SServer, " "];
                           true -> ""
                        end,
                        if SUserNonEmpty == true -> [" and with_user = ", SUser, " "];
                           true -> ""
                        end,
                        if SResourceNonEmpty == true -> [" and with_resource = ", SResource, " "];
                           true -> ""
                        end]
               end,
    TimeCond =
        case {Start, End} of
            {undefined, undefined} ->
                "";
            {undefined, _} ->
                SEnd = encode_timestamp(End),
                ["and utc < ", SEnd, " "];
            {_, undefined} ->
                SStart = encode_timestamp(Start),
                ["and utc = ", SStart, " "];
            _ ->
                SStart = encode_timestamp(Start),
                SEnd = encode_timestamp(End),
                ["and utc >= ", SStart, " "
                 "and utc < ", SEnd, " "]
	end,
    F =
        fun() ->
		WhereCond =
		    ["where us = ", SUS, " ",
		     "and deleted = 0 ",
		     TimeCond,
		     if WithCond /= "" -> ["and ", WithCond];
			true -> ""
		     end],
		TS = get_timestamp(),
		LJID = jlib:jid_tolower(jlib:make_jid(LUser, LServer, LResource)),
		case jlib:tolower(gen_mod:get_module_opt(LServer, ?MODULE, database_type, "")) of
		    %% MySQL has severe limitations for triggers: they cannot update the same table
		    %% they're invoked for, so we have to do that here.
		    %% However, yet another limitation is that in UPDATE MySQL cannot use the same table
		    %% in subquery which is being updated - so we have to cheat here, see
		    %% http://www.xaprb.com/blog/2006/06/23/how-to-select-from-an-update-target-in-mysql/
		    "mysql" ->
			run_sql_query(
			  ["update archive_collections "
			   "set next_id = NULL "
			   "where next_id in "
			   "(select id from "
			   "(select id from archive_collections ",
			   WhereCond, ") as x)"]),
			run_sql_query(
			  ["update archive_collections "
			   "set prev_id = NULL "
			   "where prev_id in "
			   "(select id from "
			   "(select id from archive_collections ",
			   WhereCond, ") as x)"]);
		    _ -> ok % Nothing to be done, all work should be done by trigger
		end,
		case run_sql_query(
		       ["update archive_collections "
			"set deleted = 1, "
			"subject = '', "
			"thread = '', "
			"extra = '', "
			"prev_id = NULL, "
			"next_id = NULL, "
			"change_by = ", get_jid_full_escaped(LJID), ", "
			"change_utc = ", encode_timestamp(TS), " ",
			WhereCond]) of
		    {deleted, 0} -> throw({error, ?ERR_ITEM_NOT_FOUND});
		    Res -> Res
		end,
		{result, []}
        end,
    run_sql_transaction(LServer, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   10. Replication
%%


process_local_iq_modified(From, _To, #iq{type = Type, sub_el = SubEl}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case Type of
        set ->
            {error, ?ERR_NOT_ALLOWED};
        get ->
            {xmlelement, _, _, SubEls} = SubEl,
            RSM = parse_rsm(SubEls),
            F =
                fun() ->
			{interval, Start, Stop, _} = parse_root_argument(SubEl),
			{{range, {Start1, _}, {_, _}, _}, _} = RSM,
			StartPresent = xml:get_tag_attr_s("start", SubEl) == "",
			{ok, Items, RSM_Item} =
			    if not is_integer(Start1), StartPresent ->
				    get_modified_legacy(LUser, LServer, RSM);
			       true ->
				    Req = get_combined_req(Start, Stop, RSM),
				    get_modified(LUser, LServer, Req)
			    end,
			{result, [{xmlelement, "modified",
				   [{"xmlns", ?NS_ARCHIVE}],
				   lists:append(
				     lists:map(
				       fun(AC) ->
					       change_to_xml(AC)
				       end, Items),
				     RSM_Item)}]}
                end,
            run_sql_transaction(LServer, F)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% X.x Utility functions to interact with the database

%%
%% The design is as follows:
%%
%% * For collections:
%%   1) When get_collection_id is called, ID of this collection from database is
%%      returned - if needed, the entity is created first using the information
%%      supplied in get_collection_id call (which is only partial).
%%   2) After having ID, you can call store_collection function to actually
%%      put meaningful values into it.
%%
%% * For messages:
%%     As messages are always created, only store_message is supported.
%%
%% * For prefs:
%%     Just store_ functions are provided, as the info is the key on its own.
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% get_collection_id related functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Adds new collection or returns ID of existing one.
%%
get_collection_id({LUser, LServer, JID, Start}) ->
    SUS = get_us_escaped({LUser, LServer}),
    SJID = get_jid_escaped(JID),
    {SUser, SServer, SResource} = SJID,
    SUTC = encode_timestamp(Start),
    case get_collection_id_raw(SUS, SJID, SUTC) of
	%% Collection is present already - just return its ID
        ID when is_integer(ID) -> ID;
        _ ->
	    %% Insert new collection.
            InsVals = [SUS, ",",
                       SUser, ",",
                       SServer, ",",
                       SResource, ",",
                       SUTC, ",",
                       "0"],
            run_sql_query(["insert into archive_collections"
                           "(us, with_user, with_server, with_resource, utc, deleted) "
                           "values(", InsVals, ")"]),
            case get_last_inserted_id(LServer, "archive_collections") of
                error -> get_collection_id_raw(SUS, SJID, SUTC);
                ID -> ID
            end
    end.

get_collection_id_raw(SUS, {SUser, SServer, SResource}, SUTC) ->
    case run_sql_query(["select id from archive_collections "
                        "where us = ", SUS, " "
                        "and with_user = ", SUser, " "
                        "and with_server = ", SServer," ",
                        "and with_resource = ", SResource, " "
                        "and utc = ", SUTC]) of
        {selected, _, Rs} when Rs /= [] ->
	    {ID} = lists:last(lists:sort(Rs)),
	    decode_integer(ID);
        _ -> {error, ?ERR_BAD_REQUEST}
    end.

%%
%% The following functions deal with links that can be present in collections.
%%
get_link_as_list([{xmlelement, Tag, Attrs, _} | _], Name)
  when Tag == Name ->
    if Attrs /= [] ->
	    {jlib:jid_tolower(jlib:string_to_jid(xml:get_attr_s("with", Attrs))),
	     get_seconds_from_datetime_string(xml:get_attr_s("start", Attrs))};
       true ->
	    []
    end;

get_link_as_list([], _) -> undefined;

get_link_as_list([_ | Tail], Name) -> get_link_as_list(Tail, Name).

get_collection_link_id({LUser, LServer}, {With, Start}) ->
    get_collection_id({LUser, LServer, With, Start});

get_collection_link_id({_, _}, _) -> null.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Store functions.
%% These functions update collections, messages or preferences respectively
%% that exist already in database (or, for prefs, possibly creating them).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_collection(C) ->
    US = C#archive_collection.us,
    {_, LServer} = US,
    JID = C#archive_collection.jid,
    %% We assume that the only part of JID store_collection can change
    %% is resource, after figuring out conversation's recipient actual resource.
    %% Currently it's not needed anymore as update_collection_partial() is provided,
    %% but we still leave it here just in case.
    {_, _, SResource} = get_jid_escaped(JID),
    SCHUTC = encode_timestamp(C#archive_collection.change_utc),
    SByJID = get_jid_full_escaped(C#archive_collection.change_by),
    CPrevID = get_collection_link_id(US, C#archive_collection.prev),
    CNextID = get_collection_link_id(US, C#archive_collection.next),
    SSubject = escape_str(LServer, C#archive_collection.subject),
    SThread = escape_str(LServer, C#archive_collection.thread),
    SExtra = escape_str(LServer, C#archive_collection.extra),
    CollVals = ["with_resource = ", SResource, ", "
                "deleted = 0, "
                "change_by = ", SByJID, ", "
                "change_utc = ", SCHUTC, ", "
                "prev_id = ", escape(CPrevID), ", "
                "next_id = ", escape(CNextID), ", "
                "subject = ", SSubject, ", "
                "thread = ", SThread, ", "
                "extra = ", SExtra],
    run_sql_query(["update archive_collections set ",
                   CollVals, " where id = ",
                   escape(C#archive_collection.id)]).

%%
%% partial collection update, useful for quick update when autosaving
%%
update_collection_partial(CID, LServer, Thread, Subject, NewRes, LJID, TS) ->
    SResource = escape(NewRes),
    SByJID = get_jid_full_escaped(LJID),
    SCHUTC = encode_timestamp(TS),
    SSubject = escape_str(LServer, Subject),
    SThread = escape_str(LServer, Thread),
    CollVals = ["with_resource = ", SResource, ", "
                "change_by = ", SByJID, ", "
                "change_utc = ", SCHUTC, ", "
                "subject = ", SSubject, ", "
                "thread = ", SThread],
    run_sql_query(["update archive_collections set ",
                   CollVals, " where id = ",
                   escape(CID)]).


%% store_message is somewhat special as it is never called for existing message -
%% therefore we can optimize it by using only one INSERT command, unlike
%% collections and changes, where we have to do SELECT -> [INSERT -> SELECT] -> UPDATE
store_message(LServer, Msg) ->
    run_sql_query([get_store_msg_header(), get_message_values_stmt(LServer, Msg)]).

%%
%% Stores multiple messages using multiple insert SQL operator - for those RDBMS'es that
%% support this syntax
%%
store_messages(LServer, CID, Msgs) ->
    case jlib:tolower(gen_mod:get_module_opt(LServer, ?MODULE, database_type, "")) of
        "sqlite" ->
	    %% Single inserts
            lists:map(
	      fun(Msg) ->
		      Msg1 = Msg#archive_message{coll_id = CID},
		      store_message(LServer, Msg1)
	      end, Msgs);
        _ ->
	    %% Multiple inserts
            Header = get_store_msg_header(),
            Res =
                lists:foldl(
		  fun(Msg, AccIn) ->
			  Msg1 = Msg#archive_message{coll_id = CID},
			  Values = get_message_values_stmt(LServer, Msg1),
			  if AccIn == "" ->
				  Header ++ Values;
			     true ->
				  Len = lists:flatlength(AccIn) + lists:flatlength(Values),
				  if Len < ?MAX_QUERY_LENGTH ->
					  AccIn ++ "," ++ Values;
				     true ->
					  run_sql_query(AccIn),
					  Header ++ Values
				  end
			  end
		  end,
		  "", Msgs),
            if Res /= "" ->
		    run_sql_query(Res);
               true -> ok
            end
    end.

get_store_msg_header() ->
    "insert into archive_messages(coll_id, utc, dir, name, body) values".

get_message_values_stmt(LServer, Msg) ->
    SDirection = escape(case Msg#archive_message.direction of
                            to -> 1;
                            from -> 0;
                            note -> 2
                        end),
    SName = escape_str(LServer, Msg#archive_message.name),
    SBody = escape_str(LServer, Msg#archive_message.body),
    ["(", escape(Msg#archive_message.coll_id), ", ",
     encode_timestamp(Msg#archive_message.utc), ", ",
     SDirection, ", ",
     SName, ", ",
     SBody, ")"].

%% store global prefs, either creating them or updating existing ones.
store_global_prefs(GPrefs) ->
    US = GPrefs#archive_global_prefs.us,
    {_, LServer} = US,
    validate_global_prefs(LServer,
                          GPrefs#archive_global_prefs.auto_save,
                          GPrefs#archive_global_prefs.save,
                          GPrefs#archive_global_prefs.expire),
    SPrefs = escape_global_prefs(GPrefs),
    Fields = ["save", "expire", "otr",
              "method_auto", "method_local", "method_manual",
              "auto_save"],
    SUS = get_us_escaped(US),
    case run_sql_query(["select us from archive_global_prefs "
                        "where us = ", SUS]) of
        {selected, _, Rs} when Rs /= [] ->
            run_sql_query(["update archive_global_prefs set ",
                           put_commas(combine_names_vals(Fields, SPrefs)),
                           " where us = ", SUS]);
        _ ->
            run_sql_query(["insert into archive_global_prefs("
                           "us, ", put_commas(Fields), ") "
                           "values(", SUS, ", ", put_commas(SPrefs), ")"])
    end.

escape_global_prefs(GPrefs) ->
    escape_common_prefs(GPrefs#archive_global_prefs.save,
                        GPrefs#archive_global_prefs.expire,
                        GPrefs#archive_global_prefs.otr) ++
	lists:map(
	  fun(V) ->
		  case V of
		      undefined -> "null";
		      prefer -> "0";
		      concede -> "1";
		      forbid -> "2";
		      _ -> throw({error, ?ERR_BAD_REQUEST})
		  end
	  end,
	  [GPrefs#archive_global_prefs.method_auto,
	   GPrefs#archive_global_prefs.method_local,
	   GPrefs#archive_global_prefs.method_manual]) ++
	[case GPrefs#archive_global_prefs.auto_save of
	     true -> "1";
	     false -> "0";
	     undefined -> "null";
	     _ -> throw({error, ?ERR_BAD_REQUEST})
	 end].

%% store jid prefs, either creating them or updating existing ones.
store_jid_prefs(Prefs) ->
    US = Prefs#archive_jid_prefs.us,
    {_, LServer} = US,
    validate_common_prefs(LServer,
                          Prefs#archive_jid_prefs.save,
                          Prefs#archive_jid_prefs.expire),
    SPrefs = escape_jid_prefs(Prefs),
    Fields = ["save", "expire", "otr"],
    SUS = get_us_escaped(US),
    {SUser, SServer, SRes} = get_jid_escaped(Prefs#archive_jid_prefs.jid),
    case run_sql_query(["select us from archive_jid_prefs "
                        "where us = ", SUS, " "
                        "and with_user = ", SUser, " "
                        "and with_server = ", SServer, " "
                        "and with_resource = ", SRes]) of
        {selected, _, Rs} when Rs /= [] ->
            run_sql_query(["update archive_jid_prefs set ",
                           put_commas(combine_names_vals(Fields, SPrefs)),
                           " where us = ", SUS, " "
                           "and with_user = ", SUser, " "
                           "and with_server = ", SServer, " "
                           "and with_resource = ", SRes]);
        _ ->
            run_sql_query(["insert into archive_jid_prefs("
                           "us, with_user, with_server, with_resource, ",
                           put_commas(Fields), ") "
                           "values(", SUS, ", ",
                           SUser, ", ",
                           SServer, ", ",
                           SRes, ", ",
                           put_commas(SPrefs), ")"])
    end.

escape_jid_prefs(Prefs) ->
    escape_common_prefs(Prefs#archive_jid_prefs.save,
                        Prefs#archive_jid_prefs.expire,
                        Prefs#archive_jid_prefs.otr).

validate_global_prefs(LServer, AutoSave, Save, Expire) ->
    DefAutoSave = gen_mod:get_module_opt(LServer, ?MODULE, default_auto_save, false),
    EnforceDefAutoSave = gen_mod:get_module_opt(LServer, ?MODULE, enforce_default_auto_save, false),
    %% Should we enforce our default auto save policy?
    %% User is trying to change auto_save to the option other than enforced.
    if EnforceDefAutoSave and (DefAutoSave /= AutoSave) and (AutoSave /= undefined) ->
	    throw({error, ?ERR_FEATURE_NOT_IMPLEMENTED});
       true -> ok
    end,
    validate_common_prefs(LServer, Save, Expire).

validate_common_prefs(LServer, Save, Expire) ->
    DefAutoSave = gen_mod:get_module_opt(LServer, ?MODULE, default_auto_save, false),
    EnforceDefAutoSave = gen_mod:get_module_opt(LServer, ?MODULE, enforce_default_auto_save, false),
    %% Should we enforce our default auto save policy?
    if EnforceDefAutoSave and
       %% auto-save=true is enforced but user is trying to put "save" element to smth other
       %% than body (thus effectively turning saving off).
       (DefAutoSave and (Save /= body) and (Save /= undefined)) ->
	    throw({error, ?ERR_FEATURE_NOT_IMPLEMENTED});
       true -> ok
    end,
    EnforceMinExpire = gen_mod:get_module_opt(LServer, ?MODULE, enforce_min_expire, 0),
    if (Expire /= undefined) and (Expire /= infinity) and
       ((EnforceMinExpire == infinity) or (Expire < EnforceMinExpire)) ->
	    throw({error, ?ERR_FEATURE_NOT_IMPLEMENTED});
       true -> ok
    end,
    EnforceMaxExpire = gen_mod:get_module_opt(LServer, ?MODULE, enforce_max_expire, infinity),
    if (EnforceMaxExpire /= infinity) and (Expire /= undefined) and
       ((Expire == infinity) or (Expire > EnforceMaxExpire)) ->
	    throw({error, ?ERR_FEATURE_NOT_IMPLEMENTED});
       true -> ok
    end.

escape_common_prefs(Save, Expire, OTR) ->
    [case Save of
         body -> "1";
         false -> "0";
         undefined -> "null";
         _ -> throw({error, ?ERR_FEATURE_NOT_IMPLEMENTED})
     end,
     case Expire of
         infinity -> "null";
         undefined -> "null";
         N -> integer_to_list(N)
     end,
     case OTR of
         undefined -> "null";
         approve -> "0";
         concede -> "1";
         forbid -> "2";
         oppose -> "3";
         prefer -> "4";
         require -> "5";
         _ -> throw({error, ?ERR_BAD_REQUEST})
     end].

put_commas(Vals) ->
    lists:foldl(
      fun(V, AccIn) ->
	      if AccIn /= "" -> AccIn ++ ", " ++ V;
		 true -> AccIn ++ V
	      end
      end,
      "", Vals).

combine_names_vals(Names, Vals) ->
    lists:zipwith(
      fun(Name, Val) ->
	      [Name, " = ", Val]
      end, Names, Vals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% This function should return the last inserted auto-generated ID,
%% if supported by database. If not - second lookup will be performed
%% to fetch new ID. Typically this should be safe, although, probably,
%% slightly slower.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_last_inserted_id(LServer, Table) ->
    case jlib:tolower(gen_mod:get_module_opt(LServer, ?MODULE, database_type, "")) of
        "mysql" -> {selected, _, [{ID}]} = run_sql_query(["select LAST_INSERT_ID()"]),
                   decode_integer(ID);
        "sqlite" -> {selected, _, [{ID}]} = run_sql_query(["select last_insert_rowid()"]),
		    decode_integer(ID);
	"pgsql" -> {selected, _, [{ID}]} = run_sql_query(["select currval('",
                                                          Table, "_id_seq')"]), decode_integer(ID);
        _ ->
            error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Helper functions to deal with RSM and main commands restrictions.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_combined_req(Start, End, {{index, Index}, Max}) ->
    {{{index, Index}, Max}, Start, End};

get_combined_req(Start, End, {{range, {RStart, StartID}, {REnd, EndID}, Order}, Max}) ->
    StartLarger = timestamp_to_integer(Start) > timestamp_to_integer(RStart),
    StartLink = if StartLarger ->
			{Start, undefined};
                   true ->
			{RStart, StartID}
                end,
    EndSmaller = timestamp_to_integer(End) < timestamp_to_integer(REnd),
    EndLink = if EndSmaller ->
		      {End, undefined};
                 true ->
		      {REnd, EndID}
              end,
    {{{range, StartLink, EndLink, Order}, Max}, Start, End};

get_combined_req(Start, End, []) ->
    {{{range, {Start, undefined}, {End, undefined}, normal}, undefined}, Start, End};

get_combined_req(_, _, _) ->
    throw({error, ?ERR_BAD_REQUEST}).

reverse_items_if_needed(Items, {{range, {_, _}, {_, _}, reversed}, _}) -> lists:reverse(Items);
reverse_items_if_needed(Items, _) -> Items.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Helper functions with common code for SQL queries
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_request_part_times(UTCField, {Start, StartID}, {End, EndID}) ->
    %% If IDs are specified - always use strict comparisons, as "non-strictness" will be added by IDs.
    %% If start is from command attribute - it should be ">=" according to XEP-136.
    %% However, if we're called for "modified" - use ">", not ">=".
    GTS = if UTCField == "change_utc"; StartID /= undefined -> ">"; true -> ">=" end,
    SStart = encode_timestamp(Start),
    SEnd = encode_timestamp(End),
    StartCond = [UTCField, " ", GTS, " ", SStart, " "],
    EndCond = [UTCField, " < ", SEnd, " "],
    [if StartID == undefined ->
	     ["and ", StartCond, " "];
        true ->
	     ["and (", StartCond, " or (", UTCField, " = ", SStart, " "
	      "and id > ", escape(StartID), ")) "] end,
     if EndID == undefined ->
	     ["and ", EndCond, " "];
        true ->
	     ["and (", EndCond, " or (", UTCField, " = ", SEnd, " "
	      "and id < ", escape(EndID), ")) "] end].

get_request_part_range(UTCField, {{range, {Start, StartID}, {End, EndID}, Order}, Max}) ->
    [get_request_part_times(UTCField, {Start, StartID}, {End, EndID}),
     "order by ", UTCField, " ",
     if Order == reversed -> "desc, "; true -> ", " end,
     "id ",
     if Order == reversed -> "desc "; true -> "" end,
     if Max /= undefined -> ["limit ", escape(Max)]; true -> "" end];

get_request_part_range(UTCField, {{index, Index}, Max}) ->
    ["order by ", UTCField, ", id ",
     if Max /= undefined -> ["limit ", escape(Max)]; true -> "" end,
     "offset ", escape(Index)].


%%
%% This function returns collections links that satisfy request restrictions
%%
get_collections_links(LUser, LServer, {RSM, Start, End}, JID) ->
    SUS = get_us_escaped({LUser, LServer}),
    SJID = if JID /= undefined -> get_jid_escaped(JID); true -> {undefined, undefined, undefined} end,
    Count = get_collections_links_count(SUS, SJID, {Start, undefined}, {End, undefined}),
    if Count == 0 ->
	    {ok, [], []};
       true ->
	    Links = get_collections_links_query(SUS, SJID, RSM),
	    CHTime = get_datetime_string_from_seconds(get_collections_links_change_time(SUS, SJID, Start, End)),
	    if Links == [] ->
		    {ok, [], make_rsm(undefined, undefined, undefined, CHTime, Count)};
	       true ->
		    Links1 = reverse_items_if_needed(Links, RSM),
		    [{FirstID, _FirstJID, FirstUTC} | _] = Links1,
		    {LastID, _LastJID, LastUTC} = lists:last(Links1),
		    FirstIndex = get_collections_links_count(SUS, SJID, {Start, undefined},
							     {FirstUTC, FirstID}),
		    {ok, Links1, make_rsm(FirstIndex,
					  make_rsm_range_item(FirstUTC, FirstID),
					  make_rsm_range_item(LastUTC, LastID),
					  CHTime,
					  Count)}
	    end
    end.

get_collections_link_req_where(SUS, {SUser, SServer, SResource}, AlsoDeleted) ->
    ServerNonEmpty = is_non_empty(SServer),
    UserNonEmpty = is_non_empty(SUser),
    ResNonEmpty = is_non_empty(SResource),
    ["where us = ", SUS, " ",
     if AlsoDeleted -> "";
        true -> "and deleted = 0 "
     end,
     if ServerNonEmpty == true -> ["and with_server = ", SServer, " "]; true -> "" end,
     if UserNonEmpty == true -> ["and with_user = ", SUser, " "]; true -> "" end,
     if ResNonEmpty == true -> ["and with_resource = ", SResource, " "]; true -> "" end].

get_collections_links_count(SUS, SJID, Start, End) ->
    get_collections_links_count_tmpl(SUS, SJID, "utc", false, Start, End).

get_collections_links_count_tmpl(SUS, SJID, Field, AlsoDeleted, {Start, StartID}, {End, EndID}) ->
    {selected, _, [{Count}]} =
        run_sql_query(["select count(*) from archive_collections ",
                       get_collections_link_req_where(SUS, SJID, AlsoDeleted),
                       get_request_part_times(Field, {Start, StartID}, {End, EndID})]),
    decode_integer(Count).

get_collections_links_query(SUS, SJID, RSM) ->
    case run_sql_query(["select id, with_user, with_server, with_resource, utc "
                        "from archive_collections ",
                        get_collections_link_req_where(SUS, SJID, false),
                        get_request_part_range("utc", RSM)]) of
        {selected, _, Rs} -> get_collections_links_list(Rs)
    end.

get_collections_links_change_time(SUS, SJID, Start, End) ->
    case run_sql_query(["select max(change_utc) from archive_collections ",
                        get_collections_link_req_where(SUS, SJID, false),
                        get_request_part_times("utc", {Start, undefined}, {End, undefined})]) of
        {selected, _, [{CHTime}]} -> decode_timestamp(CHTime)
    end.

get_collections_links_list(CLs) ->
    lists:map(fun(CL) -> get_collection_link_from_query_result(CL) end, CLs).

get_collection_link_from_query_result({ID, User, Server, Resource, UTC}) ->
    %% We do not create a full-blown record here as we do not have enough info - and
    %% just do not need it.
    {decode_integer(ID),
     jlib:jid_tolower(jlib:make_jid(User, Server, Resource)),
     decode_timestamp(UTC)}.

collection_link_to_xml(Name, {_, JID, UTC}) -> collection_link_to_xml(Name, {JID, UTC});

collection_link_to_xml(Name, {JID, UTC}) ->
    {xmlelement, Name,
     [{"with", jlib:jid_to_string(JID)},
      {"start", get_datetime_string_from_seconds(UTC)}],
     []};

collection_link_to_xml(_, _) -> [].


%%
%% This function returns full collection given its link
%% If several collections exist (which would violate XEP-136,
%% but seems to be still possible, though highly unlikely)
%% it returns the first one of them.
%%
get_collection({LUser, LServer, JID, Start}) ->
    SUS = get_us_escaped({LUser, LServer}),
    {SUser, SServer, SRes} = get_jid_escaped(JID),
    SUTC = encode_timestamp(Start),
    case run_sql_query(["select * "
                        "from archive_collections "
                        "where us = ", SUS, " "
                        "and deleted = 0 "
                        "and with_user = ", SUser, " "
                        "and with_server = ", SServer, " "
                        "and with_resource = ", SRes, " "
                        "and utc = ", SUTC]) of
        {selected, _, [C | _]} -> get_collection_from_query_result(C);
        _ -> throw({error, ?ERR_ITEM_NOT_FOUND})
    end.

get_collection_by_id(CID) ->
    {selected, _, [C | _]} = run_sql_query(["select * "
                                            "from archive_collections "
                                            "where id = ", escape(CID)]),
    get_collection_from_query_result(C).

get_collection_from_query_result({CID, PrevId, NextId, US, User, Server, Resource, UTC,
                                  ChBy, ChUTC, Deleted, Subject, Thread, Crypt, Extra}) ->
    #archive_collection{id = decode_integer(CID),
                        us = get_us_separated(US),
                        jid = jlib:jid_tolower(jlib:make_jid(User, Server, Resource)),
                        utc = decode_timestamp(UTC),
                        prev = get_collection_link_by_id(decode_integer(PrevId)),
                        next = get_collection_link_by_id(decode_integer(NextId)),
                        change_by = case ChBy of
                                        null -> {undefined, undefined, undefined};
                                        R -> jlib:jid_tolower(jlib:string_to_jid(R))
                                    end,
                        change_utc = case ChUTC of
                                         null -> undefined;
                                         R -> decode_timestamp(R)
                                     end,
                        deleted = case decode_integer(Deleted) of
                                      0 -> false;
                                      1 -> true;
                                      _ -> throw({error, ?ERR_INTERNAL_SERVER_ERROR})
                                  end,
                        subject = case Subject of
                                      null -> undefined;
                                      R -> R
                                  end,
                        thread = case Thread of
                                     null -> undefined;
                                     R -> R
                                 end,
                        crypt = case decode_integer(Crypt) of
                                    null -> false;
                                    R -> R == 1
                                end,
                        extra = case Extra of
                                    null -> undefined;
                                    R -> R
                                end}.

get_collection_link_by_id(null) -> [];

get_collection_link_by_id(CID) ->
    {selected, _, [{_, User, Server, Resource, UTC} | _]} =
        run_sql_query(["select id, with_user, with_server, with_resource, utc "
                       "from archive_collections "
                       "where id = ", escape(CID)]),
    {jlib:jid_tolower(jlib:make_jid(User, Server, Resource)), decode_timestamp(UTC)}.

collection_to_xml(C) ->
    PrevLink = collection_link_to_xml("previous", C#archive_collection.prev),
    NextLink = collection_link_to_xml("next", C#archive_collection.next),
    PrevXML = if PrevLink /= [] -> [PrevLink]; true -> [] end,
    NextXML = if NextLink /= [] -> [NextLink]; true -> [] end,
    ExtraNonEmpty = is_non_empty(C#archive_collection.extra),
    ExtraXML = if ExtraNonEmpty == true -> [decode_extra(C#archive_collection.extra)]; true -> [] end,
    {xmlelement, "chat",
     lists:append([
		   [{"with", jlib:jid_to_string(C#archive_collection.jid)}],
		   [{"start", get_datetime_string_from_seconds(C#archive_collection.utc)}],
		   if C#archive_collection.subject /= "",
		      C#archive_collection.subject /= undefined ->
			   [{"subject", C#archive_collection.subject}];
		      true ->
			   []
		   end,
		   if C#archive_collection.thread /= "",
		      C#archive_collection.thread /= undefined ->
			   [{"thread", C#archive_collection.thread}];
		      true ->
			   []
		   end,
		   if C#archive_collection.crypt -> [{"crypt", "true"}];
		      true -> []
		   end]),
     lists:append([
		   PrevXML,
		   NextXML,
		   ExtraXML])}.


%%
%% This function returns messages that satisfy request restrictions
%%
get_messages(C, RSM) ->
    CID = C#archive_collection.id,
    Count = get_messages_count(CID, {0, undefined}, {infinity, undefined}),
    if Count == 0 ->
	    {ok, [], []};
       true ->
	    Msgs = get_messages_query(CID, RSM),
	    CHTime = get_datetime_string_from_seconds(C#archive_collection.change_utc),
	    if Msgs == [] ->
		    {ok, [], make_rsm(undefined, undefined, undefined, CHTime, Count)};
	       true ->
		    Msgs1 = reverse_items_if_needed(Msgs, RSM),
		    [FirstMsg | _] = Msgs1,
		    LastMsg = lists:last(Msgs1),
		    {FirstUTC, FirstID} = {FirstMsg#archive_message.utc, FirstMsg#archive_message.id},
		    {LastUTC, LastID} = {LastMsg#archive_message.utc, LastMsg#archive_message.id},
		    FirstIndex = get_messages_count(CID, {0, undefined}, {FirstUTC, FirstID}),
		    {ok, Msgs1, make_rsm(FirstIndex,
					 make_rsm_range_item(FirstUTC, FirstID),
					 make_rsm_range_item(LastUTC, LastID),
					 CHTime,
					 Count)}
	    end
    end.

get_messages_count(CID, {Start, StartID}, {End, EndID}) ->
    {selected, _, [{Count}]} =
        run_sql_query(["select count(*) from archive_messages "
                       "where coll_id = ", escape(CID), " ",
                       get_request_part_times("utc", {Start, StartID}, {End, EndID})]),
    decode_integer(Count).

get_messages_query(CID, RSM) ->
    case run_sql_query(["select * from archive_messages "
                        "where coll_id = ", escape(CID), " ",
                        get_request_part_range("utc", RSM)]) of
        {selected, _, Rs} -> get_messages_list(Rs)
    end.

get_message_from_query_result({MID, CID, UTC, Dir, Body, Name}) ->
    #archive_message{id = decode_integer(MID),
                     coll_id = decode_integer(CID),
                     utc = decode_timestamp(UTC),
                     direction = case decode_integer(Dir) of
                                     0 -> from;
                                     1 -> to;
                                     2 -> note
                                 end,
                     body = Body,
                     name = Name}.

get_messages_list(Msgs) ->
    lists:map(fun(Msg) -> get_message_from_query_result(Msg) end, Msgs).

message_to_xml(M, Start) ->
    Dir = atom_to_list(M#archive_message.direction),
    Secs = M#archive_message.utc - Start,
    {xmlelement, Dir,
     lists:append([
		   if Dir == "note"; Secs < 0 ->
			   UTCStr = get_datetime_string_from_seconds(M#archive_message.utc),
			   [{"utc", UTCStr}];
		      true -> [{"secs", integer_to_list(Secs)}]
		   end,
		   if M#archive_message.name /= "" -> [{"name", M#archive_message.name}];
		      true -> []
		   end]),
     [if Dir == "note" -> {xmlcdata, M#archive_message.body};
	 true -> {xmlelement, "body", [], [{xmlcdata, M#archive_message.body}]}
      end]}.

%%
%% This function returns modifications that satisfy request restrictions.
%%
get_modified(LUser, LServer, {RSM, Start, End}) ->
    SUS = get_us_escaped({LUser, LServer}),
    Count = get_modified_count(SUS, {Start, undefined}, {End, undefined}),
    if Count == 0 -> {ok, [], []};
       true ->
	    Changes = get_modified_raw(SUS, RSM),
	    MaxCHTime = get_datetime_string_from_seconds(get_modified_max_change_time(SUS, Start, End)),
	    if Changes == [] ->
		    {ok, [], make_rsm(undefined, undefined, undefined, MaxCHTime, Count)};
	       true ->
		    Changes1 = reverse_items_if_needed(Changes, RSM),
		    [FirstCH | _] = Changes1,
		    LastCH = lists:last(Changes1),
		    FirstUTC = FirstCH#archive_collection.change_utc,
		    FirstID = FirstCH#archive_collection.id,
		    LastUTC = LastCH#archive_collection.change_utc,
		    LastID = LastCH#archive_collection.id,
		    FirstIndex = get_modified_count(SUS, {Start, undefined},
						    {FirstUTC, FirstID}),
		    {ok, Changes1, make_rsm(FirstIndex,
					    make_rsm_range_item(FirstUTC, FirstID),
					    make_rsm_range_item(LastUTC, LastID),
					    MaxCHTime,
					    Count)}
	    end
    end.

get_modified_count(SUS, Start, End) ->
    get_collections_links_count_tmpl(SUS, {undefined, undefined, undefined},
                                     "change_utc", true, Start, End).

get_modified_raw(SUS, RSM) ->
    {selected, _, Rs} =
        run_sql_query(["select id, us, change_by, with_user, with_server, with_resource, "
                       "utc, change_utc, deleted from archive_collections ",
                       get_collections_link_req_where(SUS, {undefined, undefined, undefined}, true),
                       get_request_part_range("change_utc", RSM)]),
    lists:map(fun(Change) -> get_change_from_query_result(Change) end, Rs).

get_modified_max_change_time(SUS, Start, End) ->
    {selected, _, [{CHTime}]} =
        run_sql_query(["select max(change_utc) ",
                       "from archive_collections ",
                       get_collections_link_req_where(SUS, {undefined, undefined, undefined}, true),
                       get_request_part_times("change_utc", {Start, undefined}, {End, undefined})]),
    decode_timestamp(CHTime).

%%
%% This is implementation of replication as specified in XEP-136. As the whole concept
%% is broken (see below) you should not use it, it is provided onlt for compliance with
%% the XEP.
%%
%% !!! NOTE !!! : poor decision about "after" usage in replication in XEP-136 breaks
%% down things if there are several changes with the same time and RSM request stops
%% somewhere between them - there's no way to get all remaining items.
%%
get_modified_legacy(LUser, LServer, {{range, {Start, undefined}, {_, _}, _}, Max}) ->
    SUS = get_us_escaped({LUser, LServer}),
    Secs = get_seconds_from_datetime_string(Start),
    Count = get_modified_count(SUS, {Secs, undefined}, {infinity, undefined}),
    if Count == 0 -> {ok, [], []};
       true ->
	    Changes = get_modified_raw(SUS, {{range, {Secs, undefined}, {infinity, undefined}, normal}, Max}),
	    MaxCHTime = get_datetime_string_from_seconds(get_modified_max_change_time(SUS, Secs, infinity)),
	    if Changes == [] ->
		    {ok, [], make_rsm(undefined, undefined, undefined, MaxCHTime, Count)};
	       true ->
		    %% We do not check for reversing changes here - we do not process "before" in
		    %% legacy mode anyway.
		    [FirstCH | _] = Changes,
		    LastCH = lists:last(Changes),
		    FirstUTC = FirstCH#archive_collection.change_utc,
		    LastUTC = LastCH#archive_collection.change_utc,
		    FirstIndex = get_modified_count(SUS, {Secs, undefined},
						    {FirstUTC, undefined}),
		    {ok, Changes, make_rsm(FirstIndex,
					   get_datetime_string_from_seconds(FirstUTC),
					   get_datetime_string_from_seconds(LastUTC),
					   MaxCHTime,
					   Count)}
	    end
    end.

get_change_from_query_result({CID, US, By, User, Server, Resource, UTC, CHUTC, Deleted}) ->
    #archive_collection{id = decode_integer(CID),
                        us = get_us_separated(US),
                        change_by = jlib:jid_tolower(jlib:string_to_jid(By)),
                        jid = jlib:jid_tolower(jlib:make_jid(User, Server, Resource)),
                        utc = decode_timestamp(UTC),
                        deleted = decode_integer(Deleted),
                        change_utc = decode_timestamp(CHUTC)}.

change_to_xml(C) ->
    CHType =
        case C#archive_collection.deleted of
            1 -> "removed";
            0 -> "changed"
        end,
    {xmlelement, CHType,
     [{"with", jlib:jid_to_string(C#archive_collection.jid)},
      {"start", get_datetime_string_from_seconds(C#archive_collection.utc)},
      {"by", jlib:jid_to_string(C#archive_collection.change_by)}], []}.



%%
%% Preferences-related retrieval functions
%%

get_global_prefs(US) ->
    SUS = get_us_escaped(US),
    case run_sql_query(["select * from archive_global_prefs "
                        "where us = ", SUS]) of
        {selected, _, [C | _]} -> get_global_prefs_from_query_result(C);
        _ -> #archive_global_prefs{}
    end.

get_global_prefs_from_query_result({US, Save, Expire, OTR,
                                    MAuto, MLocal, MManual, AutoSave}) ->
    {RSave, RExpire, ROTR} = get_common_prefs_from_query_result(Save, Expire, OTR),
    #archive_global_prefs{us = get_us_separated(US),
                          save = RSave,
                          expire = RExpire,
                          otr = ROTR,
                          method_auto = get_method_from_query_result(MAuto),
                          method_local = get_method_from_query_result(MLocal),
                          method_manual = get_method_from_query_result(MManual),
                          auto_save = case decode_integer(AutoSave) of
                                          1 -> true;
                                          0 -> false;
                                          null -> undefined
                                      end}.

get_method_from_query_result(Method) ->
    case decode_integer(Method) of
        0 -> prefer;
        1 -> concede;
        2 -> forbid;
        _ -> undefined
    end.

get_jid_prefs(US, JID) ->
    SUS = get_us_escaped(US),
    {SUser, SServer, SRes} = get_jid_escaped(JID),
    case run_sql_query(["select * from archive_jid_prefs "
                        "where us = ", SUS, " "
                        "and with_user = ", SUser, " "
                        "and with_server = ", SServer, " "
                        "and with_resource = ", SRes]) of
        {selected, _, [C | _]} -> get_jid_prefs_from_query_result(C);
        _ -> #archive_jid_prefs{}
    end.

get_all_jids_prefs(US) ->
    SUS = get_us_escaped(US),
    case run_sql_query(["select * from archive_jid_prefs "
                        "where us = ", SUS]) of
        {selected, _, Rs} -> lists:map(fun(P) -> get_jid_prefs_from_query_result(P) end, Rs);
        _ -> throw({error, ?ERR_INTERNAL_SERVER_ERROR})
    end.

get_jid_prefs_from_query_result({US, User, Server, Resource, Save, Expire, OTR}) ->
    {RSave, RExpire, ROTR} =
        get_common_prefs_from_query_result(Save, Expire, OTR),
    #archive_jid_prefs{us = get_us_separated(US),
                       jid = jlib:jid_tolower(jlib:make_jid(User, Server, Resource)),
                       save = RSave,
                       expire = RExpire,
                       otr = ROTR}.

get_common_prefs_from_query_result(Save, Expire, OTR) ->
    {case decode_integer(Save) of
         1 -> body;
         0 -> false;
         _ -> undefined
     end,
     case decode_integer(Expire) of
         null -> undefined;
         N -> N
     end,
     case decode_integer(OTR) of
         0 -> approve;
         1 -> concede;
         2 -> forbid;
         3 -> oppose;
         4 -> prefer;
         5 -> require;
         _ -> undefined
     end}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Dealing with collections expiration
%%
%% TODO: looks scaring, but I do not see any other realistic way to do it
%% without involving the caller ...
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expire_collections(Host) ->
    STS = encode_timestamp(get_timestamp()),

    ExpiredByPrefJID = [get_expired_str(Host, "archive_jid_prefs.expire", "utc"), " < ", STS],
    ExpiredByPrefGlobal = [get_expired_str(Host, "archive_global_prefs.expire", "utc"), " < ", STS],
    ExpiredByDefault = case gen_mod:get_module_opt(Host, ?MODULE, default_expire, infinity) of
                           infinity -> "";
                           N -> [get_expired_str(Host, integer_to_list(N), "utc"), " < ", STS]
                       end,

    ExistsFullJID = ["exists (select * from archive_jid_prefs "
                     "where archive_collections.us = archive_jid_prefs.us "
                     "and archive_collections.with_server = archive_jid_prefs.with_server "
                     "and archive_collections.with_user = archive_jid_prefs.with_user "
                     "and archive_collections.with_resource = archive_jid_prefs.with_resource"],
    ExistsBareJID = ["exists (select * from archive_jid_prefs "
                     "where archive_collections.us = archive_jid_prefs.us "
                     "and archive_collections.with_server = archive_jid_prefs.with_server "
                     "and archive_collections.with_user = archive_jid_prefs.with_user "
                     "and archive_jid_prefs.with_resource = ''"],
    ExistsDomainJID = ["exists (select * from archive_jid_prefs "
                       "where archive_collections.us = archive_jid_prefs.us "
                       "and archive_collections.with_server = archive_jid_prefs.with_server "
                       "and archive_jid_prefs.with_user = '' "
                       "and archive_jid_prefs.with_resource = ''"],
    ExistsGlobal =  ["exists (select * from archive_global_prefs "
                     "where archive_collections.us = archive_global_prefs.us"],

    F = fun() ->
		run_sql_query([
			       "update archive_collections "
			       "set deleted = 1, "
			       "change_by = ", escape(Host), ", "
			       "change_utc = ", STS, " "
			       "where deleted = 0 and (",

			       ExistsFullJID, " and ", ExpiredByPrefJID, ") "

			       "or not ", ExistsFullJID, ") and ", ExistsBareJID, " and ", ExpiredByPrefJID, ") "

			       "or not ", ExistsFullJID, ") and not ", ExistsBareJID, ") and ", ExistsDomainJID,
			       " and ", ExpiredByPrefJID, ") "

			       "or not ", ExistsFullJID, ") and not ", ExistsBareJID, ") and not ", ExistsDomainJID, ") "
			       "and ", ExistsGlobal, " and ", ExpiredByPrefGlobal, ") ",

			       if ExpiredByDefault /= "" ->
				       ["or not ", ExistsFullJID, ") and not ", ExistsBareJID, ") and not ", ExistsDomainJID, ") "
					"and not ", ExistsGlobal, ") and ", ExpiredByDefault];
				  true -> ""
			       end,
			       ")"]),
		case gen_mod:get_module_opt(Host, ?MODULE, replication_expire, 31536000) of
		    infinity -> [];
		    N1 ->
			run_sql_query(["delete from archive_collections "
				       "where deleted = 1 "
				       "and ", get_expired_str(Host, integer_to_list(N1), "change_utc"), " < ", STS])
		end
        end,
    run_sql_transaction(Host, F).

get_expired_str(Host, ExpExpr, UTCField) ->
    case jlib:tolower(gen_mod:get_module_opt(Host, ?MODULE, database_type, "")) of
        "mysql" -> ["timestampadd(second, ", ExpExpr, ", archive_collections.", UTCField, ")"];
        "sqlite" -> ["datetime(archive_collections.", UTCField, ", '+' || ", ExpExpr, " || ' seconds')"];
        "pgsql" -> ["timestamp archive_collections.", UTCField, " + interval ", ExpExpr, " || ' seconds'"];
        _ -> throw({error, ?ERR_INTERNAL_SERVER_ERROR})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Utility functions to make database interaction easier.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Noone seems to follow standards these days :-(
%% We have to perform DB-specific escaping,as f.e. SQLite does not understand
%% '\' as escaping character (which is exactly in accordance with the standard,
%% by the way), while most other DBs do.

%% Generic, DB-independent escaping for integers and simple strings.
escape(null) ->
    "null";
escape(undefined) ->
    "null";
escape(infinity) ->
    integer_to_list(?INFINITY);
escape(Num) when is_integer(Num) ->
    integer_to_list(Num);
escape(Str) ->
    "'" ++ [escape_chars(C) || C <- Str] ++ "'".

%% DB-specific strings escaping.
escape_str(_, null) ->
    "null";
escape_str(_, undefined) ->
    "null";
escape_str(LServer, Str) ->
    case jlib:tolower(gen_mod:get_module_opt(LServer, ?MODULE, database_type, "")) of
        "sqlite" -> "'" ++ [escape_chars(C) || C <- Str] ++ "'";
	_ -> "'" ++ ejabberd_odbc:escape(Str) ++ "'"
    end.

%% Characters to escape
escape_chars($')  -> "''";
escape_chars(C)  -> C.

%% Assume that if there are no sub-elements for "x" tag - this is
%% extra info removal request
encode_extra({xmlelement, "x", _, []}) ->
    "";
%% We could try to use BLOBs here, but base64 in text columns should
%% be more porable and should be enough - it's unlikely someone
%% will store much info here anyway.
encode_extra(Extra) ->
    jlib:encode_base64(binary_to_list(term_to_binary(Extra))).

decode_extra(Extra) ->
    binary_to_term(list_to_binary(jlib:decode_base64(Extra))).

encode_timestamp(infinity) ->
    escape(get_sql_datetime_string_from_seconds(?INFINITY));

encode_timestamp(TS) ->
    escape(get_sql_datetime_string_from_seconds(TS)).

decode_timestamp(Str) ->
    get_seconds_from_sql_datetime_string(Str).

timestamp_to_integer(infinity) ->
    ?INFINITY;
timestamp_to_integer(Num) ->
    Num.

get_us_escaped({LUser, LServer}) ->
    escape(LUser ++ "@" ++ LServer).

get_us_separated(US) ->
    JID = jlib:string_to_jid(US),
    #jid{luser = LUser, lserver = LServer} = JID,
    {LUser, LServer}.

get_jid_escaped({LUser, LServer, LResource}) ->
    {escape(LUser), escape(LServer), escape(LResource)}.

get_jid_full_escaped({LUser, LServer, undefined}) ->
    escape(LUser ++ "@" ++ LServer);
get_jid_full_escaped({LUser, LServer, ""}) ->
    escape(LUser ++ "@" ++ LServer);
get_jid_full_escaped({LUser, LServer, LResource}) ->
    escape(LUser ++ "@" ++ LServer ++ "/" ++ LResource).

decode_integer(Val) when is_integer(Val) ->
    Val;
decode_integer(null) ->
    null;
decode_integer(Val) ->
    list_to_integer(Val).

is_non_empty(null) -> false;
is_non_empty(undefined) -> false;
is_non_empty("") -> false;
is_non_empty("''") -> false;
is_non_empty(_) -> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Date-time handling.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_seconds_from_datetime_string(Str) ->
    case jlib:datetime_string_to_timestamp(Str) of
        undefined -> throw({error, ?ERR_BAD_REQUEST});
        No ->
	    calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(No))
    end.

get_datetime_string_from_seconds(Secs) ->
    Zero = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Secs2 = Secs - Zero,
    jlib:now_to_utc_string({Secs2 div 1000000, Secs2 rem 1000000, 0}).

get_seconds_from_sql_datetime_string(Str) ->
    case sql_datetime_string_to_timestamp(Str) of
        undefined -> throw({error, ?ERR_BAD_REQUEST});
        No ->
	    calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(No))
    end.

get_sql_datetime_string_from_seconds(Secs) ->
    Zero = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Secs2 = Secs - Zero,
    now_to_utc_sql_datetime({Secs2 div 1000000, Secs2 rem 1000000, 0}).

%% We do not output MicroSecs as our timestamps are seconds-based anyway, also
%% it may help to be more portable between SQL servers.
now_to_utc_sql_datetime({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
		    [Year, Month, Day, Hour, Minute, Second])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Copy-paste-modified from jlib.erl, as jlib:datetime_string_to_timestamp does not tolerate SQL syntax.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 'yyyy-mm-dd hh:mm:ss[.sss]' -> {MegaSecs, Secs, MicroSecs}
sql_datetime_string_to_timestamp(TimeStr) ->
    case catch parse_sql_datetime(TimeStr) of
	{'EXIT', _Err} ->
	    undefined;
	TimeStamp ->
	    TimeStamp
    end.

parse_sql_datetime(TimeStr) ->
    [Date, Time] = string:tokens(TimeStr, " "),
    D = parse_date(Date),
    {T, MS, TZH, TZM} = parse_time(Time),
    S = calendar:datetime_to_gregorian_seconds({D, T}),
    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = (S - S1) - TZH * 60 * 60 - TZM * 60,
    {Seconds div 1000000, Seconds rem 1000000, MS}.

%% yyyy-mm-dd
parse_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
    case calendar:valid_date(Date1) of
	true ->
	    Date1;
	_ ->
	    false
    end.

%% hh:mm:ss[.sss]
parse_time(Time) ->
    [HMS | T] =  string:tokens(Time, "."),
    MS = case T of
	     [] ->
		 0;
	     [Val] ->
		 list_to_integer(string:left(Val, 6, $0))
	 end,
    [H, M, S] = string:tokens(HMS, ":"),
    {[H1, M1, S1], true} = check_list([{H, 24}, {M, 60}, {S, 60}]),
    {{H1, M1, S1}, MS, 0, 0}.

check_list(List) ->
    lists:mapfoldl(
      fun({L, N}, B)->
	      V = list_to_integer(L),
	      if
		  (V >= 0) and (V =< N) ->
		      {V, B};
		  true ->
		      {false, false}
	      end
      end, true, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% End of copy-paste-modified
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_timestamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Wrapper functions to perform queries and transactions.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_sql_query(Query) ->
    %%?MYDEBUG("running query: ~p", [lists:flatten(Query)]),
    case catch ejabberd_odbc:sql_query_t(Query) of
        {'EXIT', Err} ->
            ?ERROR_MSG("unhandled exception during query: ~p", [Err]),
            exit(Err);
        {error, Err} ->
            ?ERROR_MSG("error during query: ~p", [Err]),
            throw({error, Err});
        aborted ->
            ?ERROR_MSG("query aborted", []),
            throw(aborted);
        R -> %?MYDEBUG("query result: ~p", [R]),
	    R
    end.

run_sql_transaction(LServer, F) ->
    DBHost = gen_mod:get_module_opt(LServer, ?MODULE, db_host, LServer),
    case ejabberd_odbc:sql_transaction(DBHost, F) of
        {atomic, R} ->
	    %%?MYDEBUG("succeeded transaction: ~p", [R]),
	    R;
        {error, Err} -> {error, Err};
        E ->
            ?ERROR_MSG("failed transaction: ~p, stack: ~p", [E, process_info(self(),backtrace)]),
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

%% return either  {error, Err}  or {LUser, LServer, Jid, Start}
link_from_argument(LUser, LServer,  Elem) ->
    case parse_root_argument(Elem) of
        {error, E} ->  {error, E};
        {interval, Start, _, JID} when Start /= 0,
                                       JID /= undefined ->
            {LUser, LServer, JID, Start};
        _ -> throw({error, ?ERR_BAD_REQUEST})
    end.

%%parse commons arguments of root elements

parse_root_argument({xmlelement, _, Attrs, _}) ->
    With = xml:get_attr_s("with", Attrs),
    Start = xml:get_attr_s("start", Attrs),
    End = xml:get_attr_s("end", Attrs),
    {interval,
     if Start /= "" -> get_seconds_from_datetime_string(Start); true -> 0 end,
     if End /= "" -> get_seconds_from_datetime_string(End); true -> infinity end,
     if With /= "" -> jlib:jid_tolower(jlib:string_to_jid(With)); true -> undefined end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Result Set Management (JEP-0059)
%%
%%
-define(MY_NS_RSM, "http://jabber.org/protocol/rsm").


%% If "index" is specified, returns {{index, Index}, Max},
%% otherwise returns {{range, {StartTime, StartID}, {EndTime, EndID}, Order}, Max}
%% where Order == 'normal' means that up to Max elements should be output
%% from Start, and 'reversed' - from End respectively.

%% !!! TODO: rewrite in "parse_root_argument" style, without recursion.

parse_rsm([A | Tail]) ->
    case A of
        {xmlelement, _,  Attrs1, _} ->
            case xml:get_attr_s("xmlns", Attrs1) of
                ?MY_NS_RSM ->
                    parse_rsm(A);
                _ ->
                    parse_rsm(Tail)
            end;
        _ ->
            parse_rsm(Tail)
    end;
parse_rsm([]) ->
    {{range, {0, undefined}, {infinity, undefined}, normal}, undefined};

parse_rsm({xmlelement, "set", _, SubEls}) ->
    parse_rsm_aux(SubEls, {{range, {0, undefined}, {infinity, undefined}, normal}, undefined});

parse_rsm(_) ->
    throw({error, ?ERR_BAD_REQUEST}).

parse_rsm_aux([{xmlelement, "max", _Attrs, Contents} | Tail], Acc) ->
    case catch list_to_integer(xml:get_cdata(Contents)) of
        P when is_integer(P) ->
            case Acc of
                {Req, undefined} ->
                    parse_rsm_aux(Tail, {Req, P});
                _ ->
                    throw({error, ?ERR_BAD_REQUEST})
            end;
        _ ->
            throw({error, ?ERR_BAD_REQUEST})
    end;

parse_rsm_aux([{xmlelement, "index", _Attrs, Contents} | Tail], Acc) ->
    case catch list_to_integer(xml:get_cdata(Contents)) of
        P when is_integer(P) ->
            case Acc of
                {{range, {0, undefined}, {infinity, undefined}, normal}, Max} ->
                    parse_rsm_aux(Tail, {{index, P}, Max});
                _ ->
                    throw({error, ?ERR_BAD_REQUEST})
            end;
        _ ->
            throw({error, ?ERR_BAD_REQUEST})
    end;

parse_rsm_aux([{xmlelement, "after", _Attrs, Contents} | Tail], Acc) ->
    case Acc of
        {{range, {0, undefined}, {infinity, undefined}, normal}, Max} ->
            parse_rsm_aux(Tail, {{range, parse_rsm_range_item(xml:get_cdata(Contents)), {infinity, undefined}, normal}, Max});
        _ ->
            throw({error, ?ERR_BAD_REQUEST})
    end;

parse_rsm_aux([{xmlelement, "before", _Attrs, Contents} | Tail], Acc) ->
    case Acc of
        {{range, {0, undefined}, {infinity, undefined}, normal}, Max} ->
            BT = case xml:get_cdata(Contents) of
                     [] -> {infinity, undefined};
                     CD -> parse_rsm_range_item(CD)
		 end,
            parse_rsm_aux(Tail, {{range, {0, undefined}, BT, reversed}, Max});
        _ ->
            throw({error, ?ERR_BAD_REQUEST})
    end;

parse_rsm_aux([_ | Tail], Acc) ->
    parse_rsm_aux(Tail, Acc);
parse_rsm_aux([], Acc) ->
    Acc.

make_rsm(undefined, undefined, undefined, Changed, Count) ->
    [{xmlelement, "set", [{"xmlns", ?MY_NS_RSM}], [
					       {xmlelement, "changed", [], [{xmlcdata,  Changed}]},
					       {xmlelement, "count", [], [{xmlcdata, integer_to_list(Count)}]}]}];

make_rsm(FirstIndex, FirstId, LastId, Changed, Count) ->
    [{xmlelement, "set", [{"xmlns", ?MY_NS_RSM}], [
						{xmlelement, "first", [{"index", integer_to_list(FirstIndex)}], [{xmlcdata,  FirstId}]},
						{xmlelement, "last", [], [{xmlcdata,  LastId}]},
						{xmlelement, "changed", [], [{xmlcdata,  Changed}]},
						{xmlelement, "count", [], [{xmlcdata,  integer_to_list(Count)}]}]}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Utility functions for RSM
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_rsm_range_item(Item) ->
    Len = string:len(Item),
    Pos = string:chr(Item, $@),
    if Pos == 0 ->
	    %% It must be either bad request or stupid RSM-136 special case for replication :-(
	    %% It's not easy to distinguish between them here, so we just return at least smth,
	    %% so that it can be dealt later with.
	    {Item, undefined};
       true ->
	    %% we do not care about exact length in second "sublist", it should only be bigger than string length.
	    {list_to_integer(lists:sublist(Item, Pos - 1)), list_to_integer(lists:sublist(Item, Pos + 1, Len))}
    end.

make_rsm_range_item(UTC, ID) ->
    integer_to_list(UTC) ++ "@" ++ integer_to_list(ID).
