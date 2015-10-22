%%%-------------------------------------------------------------------
%%% File    : mod_http_upload_quota.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Quota management for HTTP File Upload (XEP-0363)
%%% Created : 15 Oct 2015 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%-------------------------------------------------------------------

-module(mod_http_upload_quota).
-author('holger@zedat.fu-berlin.de').

-define(GEN_SERVER, gen_server).
-define(PROCNAME, ?MODULE).
-define(TIMEOUT, 86400).
-define(INITIAL_TIMEOUT, 600).

-behaviour(?GEN_SERVER).
-behaviour(gen_mod).

%% gen_mod/supervisor callbacks.
-export([start_link/3,
	 start/2,
	 stop/1,
	 mod_opt_type/1]).

%% gen_server callbacks.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% ejabberd_hooks callback.
-export([handle_slot_request/5]).

-include("jlib.hrl").
-include("logger.hrl").
-include_lib("kernel/include/file.hrl").

-record(state,
	{server_host                    :: binary(),
	 access_soft_quota              :: atom(),
	 access_hard_quota              :: atom(),
	 max_days                       :: pos_integer() | infinity,
	 docroot                        :: binary(),
	 last_sweep                     :: non_neg_integer(),
	 disk_usage = dict:new()        :: term()}).

-type state() :: #state{}.

%%--------------------------------------------------------------------
%% gen_mod/supervisor callbacks.
%%--------------------------------------------------------------------

-spec start_link(binary(), atom(), gen_mod:opts())
      -> {ok, pid()} | ignore | {error, _}.

start_link(ServerHost, Proc, Opts) ->
    ?GEN_SERVER:start_link({local, Proc}, ?MODULE, {ServerHost, Opts}, []).

-spec start(binary(), gen_mod:opts()) -> {ok, _} | {ok, _, _} | {error, _}.

start(ServerHost, Opts) ->
    Proc = mod_http_upload:get_proc_name(ServerHost, ?PROCNAME),
    Spec = {Proc,
	    {?MODULE, start_link, [ServerHost, Proc, Opts]},
	    permanent,
	    3000,
	    worker,
	    [?MODULE]},
    supervisor:start_child(ejabberd_sup, Spec).

-spec stop(binary()) -> ok.

stop(ServerHost) ->
    Proc = mod_http_upload:get_proc_name(ServerHost, ?PROCNAME),
    ok = supervisor:terminate_child(ejabberd_sup, Proc),
    ok = supervisor:delete_child(ejabberd_sup, Proc).

-spec mod_opt_type(atom()) -> fun((term()) -> term()) | [atom()].

mod_opt_type(access_soft_quota) ->
    fun(A) when is_atom(A) -> A end;
mod_opt_type(access_hard_quota) ->
    fun(A) when is_atom(A) -> A end;
mod_opt_type(max_days) ->
    fun(I) when is_integer(I), I > 0 -> I;
       (infinity) -> infinity
    end;
mod_opt_type(_) ->
    [access_soft_quota, access_hard_quota, max_days].

%%--------------------------------------------------------------------
%% gen_server callbacks.
%%--------------------------------------------------------------------

-spec init({binary(), gen_mod:opts()}) -> {ok, state(), non_neg_integer()}.

init({ServerHost, Opts}) ->
    process_flag(trap_exit, true),
    AccessSoftQuota = gen_mod:get_opt(access_soft_quota, Opts,
				      fun(A) when is_atom(A) -> A end,
				      soft_upload_quota),
    AccessHardQuota = gen_mod:get_opt(access_hard_quota, Opts,
				      fun(A) when is_atom(A) -> A end,
				      hard_upload_quota),
    MaxDays = gen_mod:get_opt(max_days, Opts,
			      fun(I) when is_integer(I), I > 0 -> I;
				 (infinity) -> infinity
			      end,
			      infinity),
    DocRoot1 = gen_mod:get_module_opt(ServerHost, mod_http_upload, docroot,
				      fun iolist_to_binary/1,
				      <<"@HOME@/upload">>),
    DocRoot2 = mod_http_upload:expand_home(str:strip(DocRoot1, right, $/)),
    LastSweep = secs_since_epoch() - ?TIMEOUT + ?INITIAL_TIMEOUT,
    ejabberd_hooks:add(http_upload_slot_request, ServerHost, ?MODULE,
		       handle_slot_request, 50),
    {ok, #state{server_host = ServerHost,
		access_soft_quota = AccessSoftQuota,
		access_hard_quota = AccessHardQuota,
		max_days = MaxDays,
		docroot = DocRoot2,
		last_sweep = LastSweep},
     ?INITIAL_TIMEOUT * 1000}.

-spec handle_call(_, {pid(), _}, state())
      -> {noreply, state(), non_neg_integer()}.

handle_call(Request, From, State) ->
    ?ERROR_MSG("Got unexpected request from ~p: ~p", [From, Request]),
    {noreply, State, remaining_timeout(State)}.

-spec handle_cast(_, state()) -> {noreply, state(), non_neg_integer()}.

handle_cast({handle_slot_request, #jid{user = U, server = S} = JID, Path, Size},
	    #state{server_host = ServerHost,
		   access_soft_quota = AccessSoftQuota,
		   access_hard_quota = AccessHardQuota,
		   disk_usage = DiskUsage} = State) ->
    HardQuota = case acl:match_rule(ServerHost, AccessHardQuota, JID) of
		  Hard when is_integer(Hard), Hard >= 0 ->
		      Hard * 1024 * 1024;
		  _ ->
		      0
		end,
    SoftQuota = case acl:match_rule(ServerHost, AccessSoftQuota, JID) of
		  Soft when is_integer(Soft), Soft >= 0 ->
		      Soft * 1024 * 1024;
		  _ ->
		      0
		end,
    OldSize = case dict:find({U, S}, DiskUsage) of
		{ok, Value} ->
		    Value;
		error ->
		    undefined
	      end,
    NewSize = case {HardQuota, SoftQuota} of
		{0, 0} ->
		    ?DEBUG("No quota specified for ~s",
			   [jlib:jid_to_string(JID)]),
		    Size;
		{0, _} ->
		      ?WARNING_MSG("No hard quota specified for ~s",
				   [jlib:jid_to_string(JID)]),
		      enforce_quota(Path, Size, OldSize, SoftQuota, SoftQuota);
		{_, 0} ->
		      ?WARNING_MSG("No soft quota specified for ~s",
				   [jlib:jid_to_string(JID)]),
		      enforce_quota(Path, Size, OldSize, HardQuota, HardQuota);
		_ when SoftQuota > HardQuota ->
		      ?WARNING_MSG("Bad quota for ~s (soft: ~p, hard: ~p)",
				   [jlib:jid_to_string(JID),
				    SoftQuota, HardQuota]),
		      enforce_quota(Path, Size, OldSize, SoftQuota, SoftQuota);
		_ ->
		      ?DEBUG("Enforcing quota for ~s",
			     [jlib:jid_to_string(JID)]),
		      enforce_quota(Path, Size, OldSize, SoftQuota, HardQuota)
	      end,
    {noreply, State#state{disk_usage = dict:store({U, S}, NewSize, DiskUsage)}};
handle_cast(Request, State) ->
    ?ERROR_MSG("Got unexpected request: ~p", [Request]),
    {noreply, State, remaining_timeout(State)}.

-spec handle_info(timeout | _, state())
      -> {noreply, state(), non_neg_integer()}.

handle_info(timeout, #state{max_days = infinity} = State) ->
    {noreply, State, remaining_timeout(State)};
handle_info(timeout, #state{docroot = DocRoot, max_days = MaxDays} = State) ->
    ?DEBUG("Got timeout message", []),
    Now = secs_since_epoch(),
    case file:list_dir(DocRoot) of
      {ok, Entries} ->
	  BackThen = Now - (MaxDays * 86400),
	  DocRootS = binary_to_list(DocRoot),
	  UserDirs = [DocRootS ++ "/" ++ Entry || Entry <- Entries,
						  filelib:is_dir(Entry)],
	  lists:foreach(fun(UserDir) -> delete_old_files(UserDir, BackThen) end,
			UserDirs);
      {error, Error} ->
	  ?ERROR_MSG("Cannot open document root ~s: ~p", [DocRoot, Error])
    end,
    NewState = State#state{last_sweep = Now},
    {noreply, NewState, remaining_timeout(NewState)};
handle_info(Info, State) ->
    ?ERROR_MSG("Got unexpected info: ~p", [Info]),
    {noreply, State, remaining_timeout(State)}.

-spec terminate(normal | shutdown | {shutdown, _} | _, _) -> ok.

terminate(Reason, #state{server_host = ServerHost}) ->
    ?DEBUG("Stopping upload quota process for ~s: ~p", [ServerHost, Reason]),
    ejabberd_hooks:delete(http_upload_slot_request, ServerHost, ?MODULE,
			  handle_slot_request, 50).

-spec code_change({down, _} | _, state(), _) -> {ok, state()}.

code_change(_OldVsn, #state{server_host = ServerHost} = State, _Extra) ->
    ?DEBUG("Updating upload quota process for ~s", [ServerHost]),
    {ok, State}.

%%--------------------------------------------------------------------
%% ejabberd_hooks callbacks.
%%--------------------------------------------------------------------

-spec handle_slot_request(term(), jid(), binary(), non_neg_integer(), binary())
      -> term().

handle_slot_request(allow, #jid{lserver = ServerHost} = JID, Path, Size,
		    _Lang) ->
    Proc = mod_http_upload:get_proc_name(ServerHost, ?PROCNAME),
    ?GEN_SERVER:cast(Proc, {handle_slot_request, JID, Path, Size}),
    allow;
handle_slot_request(Acc, _JID, _Path, _Size, _Lang) -> Acc.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------

-spec enforce_quota(file:filename_all(), non_neg_integer(),
		    non_neg_integer() | undefined, non_neg_integer(),
		    non_neg_integer())
      -> non_neg_integer().

enforce_quota(UserDir, SlotSize, OldSize, MinSize, MaxSize)
    when is_integer(OldSize), OldSize + SlotSize =< MaxSize ->
    OldSize + SlotSize;
enforce_quota(UserDir, SlotSize, _OldSize, MinSize, MaxSize) ->
    Files = lists:sort(fun({_PathA, _SizeA, TimeA}, {_PathB, _SizeB, TimeB}) ->
			       TimeA > TimeB
		       end, gather_file_info(UserDir)),
    {DelFiles, OldSize, NewSize} =
	lists:foldl(fun({Path, Size, _Time}, {[], AccSize, AccSize})
			    when AccSize + Size + SlotSize =< MinSize ->
			    {[], AccSize + Size, AccSize + Size};
		       ({Path, Size, _Time}, {[], AccSize, AccSize}) ->
			    {[Path], AccSize + Size, AccSize};
		       ({Path, Size, _Time}, {AccFiles, AccSize, NewSize}) ->
			    {[Path | AccFiles], AccSize + Size, NewSize}
		    end, {[], 0, 0}, Files),
    if OldSize + SlotSize > MaxSize ->
	    lists:foreach(fun(File) -> del_file_and_dir(File) end, DelFiles),
	    file:del_dir(UserDir), % In case it's empty, now.
	    NewSize + SlotSize;
       true ->
	    OldSize + SlotSize
    end.

-spec delete_old_files(file:filename_all(), integer()) -> ok.

delete_old_files(UserDir, Timestamp) ->
    case lists:filter(fun({_Path, _Size, Time}) -> Time < Timestamp end,
		      gather_file_info(UserDir)) of
      [] ->
	  ok;
      OldFiles ->
	  lists:foreach(fun(File) -> del_file_and_dir(File) end, OldFiles),
	  file:del_dir(UserDir) % In case it's empty, now.
    end.

-spec gather_file_info(file:filename_all())
      -> [{binary(), non_neg_integer(), non_neg_integer()}].

gather_file_info(Dir) when is_binary(Dir) ->
    gather_file_info(binary_to_list(Dir));
gather_file_info(Dir) ->
    case file:list_dir(Dir) of
      {ok, Entries} ->
	  lists:foldl(fun(Entry, Acc) ->
			      Path = Dir ++ "/" ++ Entry,
			      case file:read_file_info(Path, [{time, posix}]) of
				{ok, #file_info{type = directory}} ->
				    gather_file_info(Path) ++ Acc;
				{ok, #file_info{type = regular,
						mtime = Time,
						size = Size}} ->
				    [{Path, Size, Time} | Acc];
				{ok, _Info} ->
				    ?DEBUG("Won't stat(2) non-regular file ~s",
					   [Path]),
				    Acc;
				{error, Error} ->
				    ?ERROR_MSG("Cannot stat(2) ~s: ~s",
					       [Path, Error]),
				    Acc
			      end
		      end, [], Entries);
      {error, enoent} ->
	  ?DEBUG("Directory ~s doesn't exist", [Dir]),
	  [];
      {error, Error} ->
	  ?ERROR_MSG("Cannot open directory ~s: ~p", [Dir, Error]),
	  []
    end.

-spec del_file_and_dir(file:name_all()) -> ok.

del_file_and_dir(File) ->
    case file:delete(File) of
      ok ->
	  ?INFO_MSG("Removed ~s", [File]),
	  Dir = filename:dirname(File),
	  case file:del_dir(Dir) of
	    ok ->
		?DEBUG("Removed ~s", [Dir]);
	    {error, Error} ->
		?INFO_MSG("Cannot remove ~s: ~s", [Dir, Error])
	  end;
      {error, Error} ->
	  ?WARNING_MSG("Cannot remove ~s: ~s", [File, Error])
    end.

-spec remaining_timeout(state()) -> non_neg_integer() | infinity.

remaining_timeout(#state{max_days = infinity}) ->
    infinity;
remaining_timeout(#state{last_sweep = LastSweep}) ->
    Diff = secs_since_epoch() - LastSweep,
    if Diff > ?TIMEOUT;
       Diff < 0 ->
	    0;
       true ->
	    (?TIMEOUT - Diff) * 1000
    end.

-spec secs_since_epoch() -> non_neg_integer().

secs_since_epoch() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
