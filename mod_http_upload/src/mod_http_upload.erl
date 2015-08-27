%%%-------------------------------------------------------------------
%%% File    : mod_http_upload.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : HTTP File Upload (XEP-0363)
%%% Created : 20 Aug 2015 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%-------------------------------------------------------------------

-module(mod_http_upload).
-author('holger@zedat.fu-berlin.de').

-define(GEN_SERVER, gen_server).
-define(NS_HTTP_UPLOAD, <<"urn:xmpp:http:upload">>).
-define(NS_HTTP_UPLOAD_OLD, <<"eu:siacs:conversations:http:upload">>).
-define(SERVICE_REQUEST_TIMEOUT, 5000). % 5 seconds.
-define(SLOT_TIMEOUT, 600000). % 10 minutes.
-define(PROCNAME, ?MODULE).
-define(URL_ENC(URL), binary_to_list(ejabberd_http:url_encode(URL))).
-define(ADDR_TO_STR(IP), ejabberd_config:may_hide_data(jlib:ip_to_list(IP))).
-define(DEFAULT_CONTENT_TYPE, <<"application/octet-stream">>).
-define(CONTENT_TYPES,
	[{<<".avi">>, <<"video/avi">>},
	 {<<".bmp">>, <<"image/bmp">>},
	 {<<".bz2">>, <<"application/x-bzip2">>},
	 {<<".gif">>, <<"image/gif">>},
	 {<<".gz">>, <<"application/x-gzip">>},
	 {<<".html">>, <<"text/html">>},
	 {<<".jpeg">>, <<"image/jpeg">>},
	 {<<".jpg">>, <<"image/jpeg">>},
	 {<<".mp3">>, <<"audio/mpeg">>},
	 {<<".mp4">>, <<"video/mp4">>},
	 {<<".mpeg">>, <<"video/mpeg">>},
	 {<<".mpg">>, <<"video/mpeg">>},
	 {<<".ogg">>, <<"application/ogg">>},
	 {<<".pdf">>, <<"application/pdf">>},
	 {<<".png">>, <<"image/png">>},
	 {<<".rtf">>, <<"application/rtf">>},
	 {<<".svg">>, <<"image/svg+xml">>},
	 {<<".tiff">>, <<"image/tiff">>},
	 {<<".txt">>, <<"text/plain">>},
	 {<<".wav">>, <<"audio/wav">>},
	 {<<".webp">>, <<"image/webp">>},
	 {<<".xz">>, <<"application/x-xz">>},
	 {<<".zip">>, <<"application/zip">>}]).

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

%% ejabberd_http callback.
-export([process/2]).

%% ejabberd_hooks callback.
-export([remove_user/2]).

-include("ejabberd.hrl").
-include("ejabberd_http.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-record(state,
	{server_host            :: binary(),
	 host                   :: binary(),
	 name                   :: binary(),
	 access                 :: atom(),
	 max_size               :: pos_integer() | infinity,
	 secret_length          :: pos_integer(),
	 jid_in_url             :: sha1 | node,
	 docroot                :: binary(),
	 put_url                :: binary(),
	 get_url                :: binary(),
	 service_url            :: binary() | undefined,
	 slots = dict:new()     :: term()}). % dict:dict() requires Erlang 17.

-type state() :: #state{}.
-type slot() :: [binary()].

%%--------------------------------------------------------------------
%% gen_mod/supervisor callbacks.
%%--------------------------------------------------------------------

-spec start_link(binary(), binary(), gen_mod:opts())
      -> {ok, pid()} | ignore | {error, _}.

start_link(ServerHost, ProcHost, Opts) ->
    Proc = gen_mod:get_module_proc(ProcHost, ?PROCNAME),
    ?GEN_SERVER:start_link({local, Proc}, ?MODULE, {ServerHost, Opts}, []).

-spec start(binary(), gen_mod:opts()) -> {ok, _} | {ok, _, _} | {error, _}.

start(ServerHost, Opts) ->
    case gen_mod:get_opt(rm_on_unregister, Opts,
			 fun(B) when is_boolean(B) -> B end,
			 true) of
      true ->
	  ejabberd_hooks:add(remove_user, ServerHost, ?MODULE,
			     remove_user, 50),
	  ejabberd_hooks:add(anonymous_purge_hook, ServerHost, ?MODULE,
			     remove_user, 50);
      false -> ok
    end,
    PutURL = gen_mod:get_opt(put_url, Opts,
			     fun(<<"http://", _/binary>> = URL) -> URL;
				(<<"https://", _/binary>> = URL) -> URL;
				(_) -> <<"http://@HOST@">>
			     end,
			     <<"http://@HOST@">>),
    [_, ProcHost | _] = binary:split(expand_host(PutURL, ServerHost),
				     [<<"http://">>, <<"https://">>,
				      <<":">>, <<"/">>], [global]),
    Proc = gen_mod:get_module_proc(ProcHost, ?PROCNAME),
    Spec = {Proc,
	    {?MODULE, start_link, [ServerHost, ProcHost, Opts]},
	    permanent,
	    3000,
	    worker,
	    [?MODULE]},
    supervisor:start_child(ejabberd_sup, Spec).

-spec stop(binary()) -> ok.

stop(ServerHost) ->
    case gen_mod:get_module_opt(ServerHost, ?MODULE, rm_on_unregister,
				fun(B) when is_boolean(B) -> B end,
			        true) of
      true ->
	  ejabberd_hooks:delete(remove_user, ServerHost, ?MODULE,
				remove_user, 50),
	  ejabberd_hooks:delete(anonymous_purge_hook, ServerHost, ?MODULE,
				remove_user, 50);
      false -> ok
    end,
    Proc = gen_mod:get_module_proc(ServerHost, ?PROCNAME),
    ok = supervisor:terminate_child(ejabberd_sup, Proc),
    ok = supervisor:delete_child(ejabberd_sup, Proc).

-spec mod_opt_type(atom()) -> fun((term()) -> term()).

mod_opt_type(host) ->
    fun iolist_to_binary/1;
mod_opt_type(name) ->
    fun iolist_to_binary/1;
mod_opt_type(access) ->
    fun(A) when is_atom(A) -> A end;
mod_opt_type(max_size) ->
    fun(I) when is_integer(I), I > 0 -> I;
        (infinity) -> infinity
    end;
mod_opt_type(secret_length) ->
    fun(I) when is_integer(I), I >= 8 -> I end;
mod_opt_type(jid_in_url) ->
    fun(sha1) -> sha1;
       (node) -> node
    end;
mod_opt_type(docroot) ->
    fun iolist_to_binary/1;
mod_opt_type(put_url) ->
    fun(<<"http://", _/binary>> = URL) -> URL;
       (<<"https://", _/binary>> = URL) -> URL
    end;
mod_opt_type(get_url) ->
    fun(<<"http://", _/binary>> = URL) -> URL;
       (<<"https://", _/binary>> = URL) -> URL
    end;
mod_opt_type(service_url) ->
    fun(<<"http://", _/binary>> = URL) -> URL;
       (<<"https://", _/binary>> = URL) -> URL
    end;
mod_opt_type(rm_on_unregister) ->
    fun(B) when is_boolean(B) -> B end;
mod_opt_type(_) ->
    [host, name, access, max_size, secret_length, jid_in_url, docroot,
     put_url, get_url, service_url, rm_on_unregister].

%%--------------------------------------------------------------------
%% gen_server callbacks.
%%--------------------------------------------------------------------

-spec init({binary(), gen_mod:opts()}) -> {ok, state()}.

init({ServerHost, Opts}) ->
    process_flag(trap_exit, true),
    Host = gen_mod:get_opt_host(ServerHost, Opts, <<"upload.@HOST@">>),
    Name = gen_mod:get_opt(name, Opts,
			   fun iolist_to_binary/1,
			   <<"HTTP File Upload">>),
    Access = gen_mod:get_opt(access, Opts,
			     fun(A) when is_atom(A) -> A end,
			     local),
    MaxSize = gen_mod:get_opt(max_size, Opts,
			      fun(I) when is_integer(I), I > 0 -> I;
				 (infinity) -> infinity
			      end,
			      104857600),
    SecretLength = gen_mod:get_opt(secret_length, Opts,
				   fun(I) when is_integer(I), I >= 8 -> I end,
				   40),
    JIDinURL = gen_mod:get_opt(jid_in_url, Opts,
				   fun(sha1) -> sha1;
				      (node) -> node
				   end,
				   sha1),
    DocRoot = gen_mod:get_opt(docroot, Opts,
			      fun iolist_to_binary/1,
			      <<"@HOME@/upload">>),
    PutURL = gen_mod:get_opt(put_url, Opts,
			     fun(<<"http://", _/binary>> = URL) -> URL;
				(<<"https://", _/binary>> = URL) -> URL
			     end,
			     <<"http://@HOST@:5444">>),
    GetURL = gen_mod:get_opt(get_url, Opts,
			     fun(<<"http://", _/binary>> = URL) -> URL;
				(<<"https://", _/binary>> = URL) -> URL
			     end,
			     PutURL),
    ServiceURL = gen_mod:get_opt(service_url, Opts,
				 fun(<<"http://", _/binary>> = URL) -> URL;
				    (<<"https://", _/binary>> = URL) -> URL
				 end),
    case ServiceURL of
      undefined ->
	  ok;
      <<"http://", _/binary>> ->
	  application:start(inets);
      <<"https://", _/binary>> ->
	  application:start(inets),
	  application:start(crypto),
	  application:start(public_key),
	  application:start(ssl)
    end,
    ejabberd_router:register_route(Host),
    {ok, #state{server_host = ServerHost, host = Host, name = Name,
		access = Access, max_size = MaxSize,
		secret_length = SecretLength, jid_in_url = JIDinURL,
		docroot = expand_home(str:strip(DocRoot, right, $/)),
		put_url = expand_host(str:strip(PutURL, right, $/), ServerHost),
		get_url = expand_host(str:strip(GetURL, right, $/), ServerHost),
		service_url = ServiceURL}}.

-spec handle_call(_, {pid(), _}, state()) -> {noreply, state()}.

handle_call({use_slot, Slot}, _From, #state{docroot = DocRoot} = State) ->
    case get_slot(Slot, State) of
      {ok, {Size, Timer}} ->
	  timer:cancel(Timer),
	  NewState = del_slot(Slot, State),
	  Path = str:join([DocRoot | Slot], <<$/>>),
	  {reply, {ok, Size, Path}, NewState};
      error ->
	  {reply, {error, <<"Invalid slot">>}, State}
    end;
handle_call(get_docroot, _From, #state{docroot = DocRoot} = State) ->
    {reply, {ok, DocRoot}, State};
handle_call(Request, From, State) ->
    ?ERROR_MSG("Got unexpected request from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.

handle_cast(Request, State) ->
    ?ERROR_MSG("Got unexpected request: ~p", [Request]),
    {noreply, State}.

-spec handle_info(timeout | _, state()) -> {noreply, state()}.

handle_info({route, From, To, #xmlel{name = <<"iq">>} = Stanza}, State) ->
    Request = jlib:iq_query_info(Stanza),
    {Reply, NewState} = case process_iq(From, Request, State) of
			  R when is_record(R, iq) ->
			      {R, State};
			  {R, S} ->
			      {R, S};
			  not_request ->
			      {none, State}
			end,
    if Reply /= none ->
	    ejabberd_router:route(To, From, jlib:iq_to_xml(Reply));
       true ->
	    ok
    end,
    {noreply, NewState};
handle_info({slot_timed_out, Slot}, State) ->
    NewState = del_slot(Slot, State),
    {noreply, NewState};
handle_info(Info, State) ->
    ?ERROR_MSG("Got unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, _} | _, _) -> ok.

terminate(Reason, #state{server_host = ServerHost, host = Host}) ->
    ?DEBUG("Stopping HTTP upload process for ~s: ~p", [ServerHost, Reason]),
    ejabberd_router:unregister_route(Host),
    ok.

-spec code_change({down, _} | _, state(), _) -> {ok, state()}.

code_change(_OldVsn, #state{server_host = ServerHost} = State, _Extra) ->
    ?DEBUG("Updating HTTP upload process for ~s", [ServerHost]),
    {ok, State}.

%%--------------------------------------------------------------------
%% ejabberd_http callback.
%%--------------------------------------------------------------------

-spec process([binary()], #request{})
      -> {pos_integer(), [{binary(), binary()}], binary()}.

process(LocalPath, #request{method = 'PUT', host = Host, ip = IP,
			    data = Data}) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    case catch gen_server:call(Proc, {use_slot, LocalPath}) of
      {ok, Size, Path} when byte_size(Data) == Size ->
	  ?DEBUG("Storing file from ~s for ~s: ~s",
		 [?ADDR_TO_STR(IP), Host, Path]),
	  case store_file(Path, Data) of
	    ok ->
		http_response(201);
	    {error, Error} ->
		?ERROR_MSG("Cannot store file ~s from ~s for ~s: ~s",
			   [Path, ?ADDR_TO_STR(IP), Host, Error]),
		http_response(500)
	  end;
      {ok, Size, Path} ->
	  ?INFO_MSG("Rejecting file ~s from ~s for ~s: Size is ~B, not ~B",
		    [Path, ?ADDR_TO_STR(IP), Host, byte_size(Data), Size]),
	  http_response(413);
      {error, Error} ->
	  ?INFO_MSG("Rejecting file from ~s for ~s: ~p",
		    [?ADDR_TO_STR(IP), Host, Error]),
	  http_response(403);
      Error ->
	  ?ERROR_MSG("Cannot handle PUT request from ~s for ~s: ~p",
		     [?ADDR_TO_STR(IP), Host, Error]),
	  http_response(500)
    end;
process(LocalPath, #request{method = 'GET', host = Host, ip = IP}) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    case catch gen_server:call(Proc, get_docroot) of
      {ok, DocRoot} ->
	  Path = str:join([DocRoot | LocalPath], <<$/>>),
	  case file:read_file(Path) of
	    {ok, Data} ->
		?INFO_MSG("Serving ~s to ~s", [Path, ?ADDR_TO_STR(IP)]),
		FileName = lists:last(LocalPath),
		ContentType = guess_content_type(FileName),
		Headers1 = case ContentType of
			     <<"image/", _SubType/binary>> -> [];
			     <<"text/", _SubType/binary>> -> [];
			     _ ->
				 [{<<"Content-Disposition">>,
				   <<"attachment; filename=",
				     $", FileName/binary, $">>}]
			   end,
		Headers2 = [{<<"Content-Type">>, ContentType} | Headers1],
		http_response(200, Headers2, Data);
	    {error, eacces} ->
		?INFO_MSG("Cannot serve ~s to ~s: Permission denied",
			  [Path, ?ADDR_TO_STR(IP)]),
		http_response(403);
	    {error, enoent} ->
		?INFO_MSG("Cannot serve ~s to ~s: No such file or directory",
			  [Path, ?ADDR_TO_STR(IP)]),
		http_response(404);
	    {error, eisdir} ->
		?INFO_MSG("Cannot serve ~s to ~s: Is a directory",
			  [Path, ?ADDR_TO_STR(IP)]),
		http_response(404);
	    {error, Error} ->
		?INFO_MSG("Cannot serve ~s to ~s: ~p",
			  [Path, ?ADDR_TO_STR(IP), Error]),
		http_response(500)
	  end;
      Error ->
	  ?ERROR_MSG("Cannot handle GET request from ~s for ~s: ~p",
		     [?ADDR_TO_STR(IP), Host, Error]),
	  http_response(500)
    end;
process(_LocalPath, #request{method = Method, host = Host, ip = IP}) ->
    ?DEBUG("Rejecting ~s request from ~s for ~s",
	   [Method, ?ADDR_TO_STR(IP), Host]),
    http_response(405, [{<<"Allow">>, <<"GET, PUT">>}]).

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------

%% XMPP request handling.

-spec process_iq(jid(), iq_request() | reply | invalid, state())
      -> {iq_reply(), state()} | iq_reply() | not_request.

process_iq(_From,
	   #iq{type = get, xmlns = ?NS_DISCO_INFO, lang = Lang} = IQ,
	   #state{server_host = ServerHost, name = Name}) ->
    AddInfo = ejabberd_hooks:run_fold(disco_info, ServerHost, [],
				      [ServerHost, ?MODULE, <<"">>, <<"">>]),
    IQ#iq{type = result,
	  sub_el = [#xmlel{name = <<"query">>,
			   attrs = [{<<"xmlns">>, ?NS_DISCO_INFO}],
			   children = iq_disco_info(Lang, Name) ++ AddInfo}]};
process_iq(#jid{luser = LUser, lserver = LServer} = From,
	   #iq{type = get, xmlns = XMLNS, lang = Lang, sub_el = SubEl} = IQ,
	   #state{server_host = ServerHost, access = Access} = State)
    when XMLNS == ?NS_HTTP_UPLOAD;
	 XMLNS == ?NS_HTTP_UPLOAD_OLD ->
    User = <<LUser/binary, $@, LServer/binary>>,
    case acl:match_rule(ServerHost, Access, From) of
      allow ->
	  case parse_request(SubEl, Lang) of
	    {ok, File, Size, ContentType} ->
		case create_slot(State, User, File, Size, ContentType, Lang) of
		  {ok, Slot} ->
		      {ok, Timer} = timer:send_after(?SLOT_TIMEOUT,
						     {slot_timed_out, Slot}),
		      NewState = add_slot(Slot, Size, Timer, State),
		      SlotEl = slot_el(Slot, State, XMLNS),
		      {IQ#iq{type = result, sub_el = [SlotEl]}, NewState};
		  {ok, PutURL, GetURL} ->
		      SlotEl = slot_el(PutURL, GetURL, XMLNS),
		      IQ#iq{type = result, sub_el = [SlotEl]};
		  {error, Error} ->
		      IQ#iq{type = error, sub_el = [SubEl, Error]}
		end;
	    {error, Error} ->
		?DEBUG("Cannot parse request from ~s", [User]),
		IQ#iq{type = error, sub_el = [SubEl, Error]}
	  end;
      deny ->
	  ?DEBUG("Denying HTTP upload slot request from ~s", [User]),
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
    end;
process_iq(_From, #iq{sub_el = SubEl} = IQ, _State) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
process_iq(_From, reply, _State) ->
    not_request;
process_iq(_From, invalid, _State) ->
    not_request.

-spec parse_request(xmlel(), binary())
      -> {ok, binary(), pos_integer(), binary()} | {error, xmlel()}.

parse_request(#xmlel{name = <<"request">>, attrs = Attrs} = Request, Lang) ->
    case xml:get_attr(<<"xmlns">>, Attrs) of
      {value, XMLNS} when XMLNS == ?NS_HTTP_UPLOAD;
			  XMLNS == ?NS_HTTP_UPLOAD_OLD ->
	  case {xml:get_subtag_cdata(Request, <<"filename">>),
		xml:get_subtag_cdata(Request, <<"size">>),
		xml:get_subtag_cdata(Request, <<"content-type">>)} of
	    {File, SizeStr, ContentType} when byte_size(File) > 0 ->
		case catch jlib:binary_to_integer(SizeStr) of
		  Size when is_integer(Size), Size > 0 ->
		      {ok, File, Size, yield_content_type(ContentType)};
		  _ ->
		      Text = <<"Please specify file size.">>,
		      {error, ?ERRT_BAD_REQUEST(Lang, Text)}
		end;
	    _ ->
		Text = <<"Please specify file name.">>,
		{error, ?ERRT_BAD_REQUEST(Lang, Text)}
	  end;
      _ ->
	  {error, ?ERR_BAD_REQUEST}
    end;
parse_request(_El, _Lang) -> {error, ?ERR_BAD_REQUEST}.

-spec create_slot(state(), binary(), binary(), pos_integer(), binary(),
		  binary())
      -> {ok, slot()} | {ok, binary(), binary()} | {error, xmlel()}.

create_slot(#state{service_url = undefined, max_size = MaxSize},
	    User, File, Size, _ContentType, Lang) when MaxSize /= infinity,
						       Size > MaxSize ->
    Text = <<"File larger than ", (jlib:integer_to_binary(MaxSize))/binary,
	     " Bytes.">>,
    ?INFO_MSG("Rejecting file ~s from ~s (too large: ~B bytes)",
	      [File, User, Size]),
    {error, ?ERRT_NOT_ACCEPTABLE(Lang, Text)};
create_slot(#state{service_url = undefined,
		   jid_in_url = JIDinURL,
		   secret_length = SecretLength},
	    User, File, _Size, _ContentType, _Lang) ->
    UserStr = make_user_string(User, JIDinURL),
    RandStr = make_rand_string(SecretLength),
    FileStr = make_file_string(File),
    ?INFO_MSG("Got HTTP upload slot for ~s (file: ~s)", [User, File]),
    {ok, [UserStr, RandStr, FileStr]};
create_slot(#state{service_url = ServiceURL}, User, File, Size, ContentType,
	    _Lang) ->
    Options = [{body_format, binary}, {full_result, false}],
    HttpOptions = [{timeout, ?SERVICE_REQUEST_TIMEOUT}],
    SizeStr = jlib:integer_to_binary(Size),
    GetRequest = binary_to_list(ServiceURL) ++
		     "?jid=" ++ ?URL_ENC(User) ++
		     "&name=" ++ ?URL_ENC(File) ++
		     "&size=" ++ ?URL_ENC(SizeStr) ++
		     "&content_type=" ++ ?URL_ENC(ContentType),
    case httpc:request(get, {GetRequest, []}, HttpOptions, Options) of
      {ok, {Code, Body}} when Code >= 200, Code =< 299 ->
	  case binary:split(Body, <<$\n>>, [global, trim]) of
	    [<<"http", _/binary>> = PutURL, <<"http", _/binary>> = GetURL] ->
		?INFO_MSG("Got HTTP upload slot for ~s (file: ~s)",
			  [User, File]),
		{ok, PutURL, GetURL};
	    Lines ->
		?ERROR_MSG("Cannot parse data received for ~s from <~s>: ~p",
			   [User, ServiceURL, Lines]),
		{error, ?ERR_SERVICE_UNAVAILABLE}
	  end;
      {error, {402, _Body}} ->
	  ?INFO_MSG("Got status code 402 for ~s from <~s>", [User, ServiceURL]),
	  {error, ?ERR_RESOURCE_CONSTRAINT};
      {error, {403, _Body}} ->
	  ?INFO_MSG("Got status code 403 for ~s from <~s>", [User, ServiceURL]),
	  {error, ?ERR_NOT_ALLOWED};
      {error, {413, _Body}} ->
	  ?INFO_MSG("Got status code 413 for ~s from <~s>", [User, ServiceURL]),
	  {error, ?ERR_NOT_ACCEPTABLE};
      {error, {Code, _Body}} ->
	  ?ERROR_MSG("Got unexpected status code ~s from <~s>: ~B",
		     [User, ServiceURL, Code]),
	  {error, ?ERR_SERVICE_UNAVAILABLE};
      {error, Reason} ->
	  ?ERROR_MSG("Error requesting upload slot for ~s from <~s>: ~p",
		     [User, ServiceURL, Reason]),
	  {error, ?ERR_SERVICE_UNAVAILABLE}
    end.

-spec add_slot(slot(), pos_integer(), timer:tref(), state()) -> state().

add_slot(Slot, Size, Timer, #state{slots = Slots} = State) ->
    NewSlots = dict:store(Slot, {Size, Timer}, Slots),
    State#state{slots = NewSlots}.

-spec get_slot(slot(), state()) -> {ok, {pos_integer(), timer:tref()}} | error.

get_slot(Slot, #state{slots = Slots}) ->
    dict:find(Slot, Slots).

-spec del_slot(slot(), state()) -> state().

del_slot(Slot, #state{slots = Slots} = State) ->
    NewSlots = dict:erase(Slot, Slots),
    State#state{slots = NewSlots}.

-spec slot_el(slot() | binary(), state() | binary(), binary()) -> xmlel().

slot_el(Slot, #state{put_url = PutPrefix, get_url = GetPrefix}, XMLNS) ->
    PutURL = str:join([PutPrefix | Slot], <<$/>>),
    GetURL = str:join([GetPrefix | Slot], <<$/>>),
    slot_el(PutURL, GetURL, XMLNS);
slot_el(PutURL, GetURL, XMLNS) ->
    #xmlel{name = <<"slot">>,
	   attrs = [{<<"xmlns">>, XMLNS}],
	   children = [#xmlel{name = <<"put">>,
			      children = [{xmlcdata, PutURL}]},
		       #xmlel{name = <<"get">>,
			      children = [{xmlcdata, GetURL}]}]}.

-spec make_user_string(binary(), sha1 | node) -> binary().

make_user_string(User, sha1) ->
    p1_sha:sha(User);
make_user_string(User, node) ->
    [Node, _Domain] = binary:split(User, <<$@>>),
    re:replace(Node, <<"[^a-zA-Z0-9_.-]">>, <<$_>>, [global, {return, binary}]).

-spec make_file_string(binary()) -> binary().

make_file_string(File) ->
    re:replace(File, <<"[^a-zA-Z0-9_.-]">>, <<$_>>, [global, {return, binary}]).

-spec make_rand_string(non_neg_integer()) -> binary().

make_rand_string(Length) ->
    list_to_binary(make_rand_string([], Length)).

-spec make_rand_string(string(), non_neg_integer()) -> string().

make_rand_string(S, 0) -> S;
make_rand_string(S, N) -> make_rand_string([make_rand_char() | S], N - 1).

-spec make_rand_char() -> char().

make_rand_char() ->
    map_int_to_char(crypto:rand_uniform(0, 62)).

-spec map_int_to_char(0..61) -> char().

map_int_to_char(N) when N =<  9 -> N + 48; % Digit.
map_int_to_char(N) when N =< 35 -> N + 55; % Upper-case character.
map_int_to_char(N) when N =< 61 -> N + 61. % Lower-case character.

-spec expand_home(binary()) -> binary().

expand_home(Subject) ->
    {ok, [[Home]]} = init:get_argument(home),
    Parts = binary:split(Subject, <<"@HOME@">>, [global]),
    str:join(Parts, list_to_binary(Home)).

-spec expand_host(binary(), binary()) -> binary().

expand_host(Subject, Host) ->
    Parts = binary:split(Subject, <<"@HOST@">>, [global]),
    str:join(Parts, Host).

-spec yield_content_type(binary()) -> binary().

yield_content_type(<<"">>) -> <<"application/octet-stream">>;
yield_content_type(Type) -> Type.

-spec iq_disco_info(binary(), binary()) -> [xmlel()].

iq_disco_info(Lang, Name) ->
    [#xmlel{name = <<"identity">>,
	    attrs = [{<<"category">>, <<"store">>},
		     {<<"type">>, <<"file">>},
		     {<<"name">>, translate:translate(Lang, Name)}]},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_HTTP_UPLOAD}]},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_HTTP_UPLOAD_OLD}]}].

%% HTTP request handling.

-spec store_file(file:filename_all(), binary()) -> ok | {error, term()}.

store_file(Path, Data) ->
    try
	ok = filelib:ensure_dir(Path),
	{ok, Io} = file:open(Path, [write, exclusive, raw]),
	Ok = file:write(Io, Data),
	ok = file:close(Io), % Close file even if file:write/2 failed.
	ok = Ok              % But raise an exception in that case.
    catch
      _:{badmatch, {error, Error}} ->
	  {error, Error};
      _:Error ->
	  {error, Error}
    end.

-spec guess_content_type(binary()) -> binary().

guess_content_type(FileName) ->
    mod_http_fileserver:content_type(FileName,
				     ?DEFAULT_CONTENT_TYPE,
				     ?CONTENT_TYPES).

-spec http_response(100..599)
      -> {pos_integer(), [{binary(), binary()}], binary()}.

http_response(Code) ->
    http_response(Code, []).

-spec http_response(100..599, [{binary(), binary()}])
      -> {pos_integer(), [{binary(), binary()}], binary()}.

http_response(Code, ExtraHeaders) ->
    http_response(Code, ExtraHeaders, <<(code_to_message(Code))/binary, $\n>>).

-spec http_response(100..599, [{binary(), binary()}], binary())
      -> {pos_integer(), [{binary(), binary()}], binary()}.

http_response(Code, ExtraHeaders, Body) ->
    ServerHeader = {<<"Server">>, <<"ejabberd ", (?VERSION)/binary>>},
    Headers = case proplists:is_defined(<<"Content-Type">>, ExtraHeaders) of
		true ->
		    [ServerHeader | ExtraHeaders];
		false ->
		    [ServerHeader, {<<"Content-Type">>, <<"text/plain">>} |
		     ExtraHeaders]
	      end,
    {Code, Headers, Body}.

-spec code_to_message(100..599) -> binary().

code_to_message(201) -> <<"Upload successful.">>;
code_to_message(403) -> <<"Forbidden.">>;
code_to_message(404) -> <<"Not found.">>;
code_to_message(405) -> <<"Method not allowed.">>;
code_to_message(413) -> <<"File size doesn't match requested size.">>;
code_to_message(500) -> <<"Internal server error.">>.

%%--------------------------------------------------------------------
%% Remove user.
%%--------------------------------------------------------------------

-spec remove_user(binary(), binary()) -> ok.

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    DocRoot = gen_mod:get_module_opt(LServer, ?MODULE, docroot,
				     fun iolist_to_binary/1,
				     <<"@HOME@/upload">>),
    JIDinURL = gen_mod:get_module_opt(LServer, ?MODULE, jid_in_url,
				      fun(sha1) -> sha1;
					 (node) -> node
				      end,
				      sha1),
    UserStr = make_user_string(<<LUser/binary, $@, LServer/binary>>, JIDinURL),
    UserDir = str:join([expand_home(DocRoot), UserStr], <<$/>>),
    case del_tree(UserDir) of
	ok ->
	    ?INFO_MSG("Removed HTTP upload directory of ~s@~s", [User, Server]);
	{error, enoent} ->
	    ?DEBUG("Found no HTTP upload directory of ~s@~s", [User, Server]);
	{error, Error} ->
	    ?ERROR_MSG("Cannot remove HTTP upload directory of ~s@~s: ~p",
		       [User, Server, Error])
    end,
    ok.

-spec del_tree(file:filename_all()) -> ok | {error, term()}.

del_tree(Dir) when is_binary(Dir) ->
    del_tree(binary_to_list(Dir));
del_tree(Dir) ->
    try
	{ok, Entries} = file:list_dir(Dir),
	lists:foreach(fun(Path) ->
			      case filelib:is_dir(Path) of
				true ->
				    ok = del_tree(Path);
				false ->
				    ok = file:delete(Path)
			      end
		      end, [Dir ++ "/" ++ Entry || Entry <- Entries]),
	ok = file:del_dir(Dir)
    catch
      _:{badmatch, {error, Error}} ->
	  {error, Error};
      _:Error ->
	  {error, Error}
    end.
