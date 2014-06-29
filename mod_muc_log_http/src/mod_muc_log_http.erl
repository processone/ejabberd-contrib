%%%----------------------------------------------------------------------
%%% File    : mod_muc_log_http.erl
%%% Author  : Badlop, Massimiliano Mirra
%%% Purpose : MUC logs simple file server
%%% Created :
%%% Id      :
%%%----------------------------------------------------------------------

-module(mod_muc_log_http).

-behaviour(gen_mod).

-export([
	 start/2,
	 stop/1,
	 loop/1,
	 process/2
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("mod_muc_room.hrl").
-include("logger.hrl").
-include_lib("kernel/include/file.hrl").

-define(PROCNAME, mod_muc_log_http).

-ifdef(SSL39).
-define(STRING2LOWER, string).
-else.
-define(STRING2LOWER, httpd_util).
-endif.

% TODO:
%  - If chatroom is password protected, ask password
%  - If chatroom is only for members, ask for username and password

%% Copied from mod_muc/mod_muc.erl
-record(muc_online_room, {name_host, pid}).


%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

process(LocalPath, Request) ->
	serve(LocalPath, Request).

serve(LocalPath, #request{host = Host} = Request) ->
	Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	Proc ! {get_docroot, self()},
	receive DocRoot -> ok end,
	FileName = filename:join(filename:split(DocRoot) ++ LocalPath),
	case file:read_file(FileName) of
		{ok, FileContents} ->
			?DEBUG("Delivering content.", []),
			{200,
			 [{"Server", "ejabberd"},
			  {"Last-Modified", last_modified(FileName)},
			  {"Content-Type", content_type(FileName)}],
			 FileContents};
		{error, eisdir} ->
			FileNameIndex = FileName ++ "/index.html",
			case file:read_file_info(FileNameIndex) of
				{ok, _FileInfo} -> serve(LocalPath ++ ["index.html"], Request);
				{error, _Error} -> show_dir_listing(FileName, LocalPath)
			end;
		{error, Error} ->
			?DEBUG("Delivering error: ~p", [Error]),
			case Error of
				eacces -> {403, [], "Forbidden"};
				enoent -> {404, [], "Not found"};
				_Else -> {404, [], atom_to_list(Error)}
			end
	end.


%%%----------------------------------------------------------------------
%%% Dir listing
%%%----------------------------------------------------------------------

build_datetimelist(DateTime) ->
	{{Ye, Mo, Da}, {Ho, Mi, Se}} = DateTime,
	Nums = [Mo, Da, Ho, Mi, Se],
	Nums2 = [fill(Num) || Num <- Nums],
	[integer_to_list(Ye)] ++ Nums2.

fill(Num) when Num < 10 -> io_lib:format("0~p", [Num]);
fill(Num) -> io_lib:format("~p", [Num]).


%%%----------------------------------------------------------------------
%%% MUC INFO
%%%----------------------------------------------------------------------

get_room_pid(Name, Host) ->
	case ets:lookup(muc_online_room, {Name, Host}) of
		[] -> unknown;
		[Room_ets] -> Room_ets#muc_online_room.pid
	end.

get_room_config(Room_pid) ->
	{ok, C} = gen_fsm:sync_send_all_state_event(Room_pid, get_config),
	C.


%%%----------------------------------------------------------------------
%%% MUC LIST
%%%----------------------------------------------------------------------

show_dir_listing(DirName, LocalPath) ->
	Header = io_lib:format("Name                                               Last modified             Size Description~n", []),
	Address = io_lib:format("<address>ejabberd/~s Server</address>", [?VERSION]),

	{ok, Listing} = file:list_dir(DirName),
	Listing2 = lists:sort(Listing),

	Listing3 = case LocalPath of
		[] -> 
			lists:filter(
				fun(RoomFull) ->
					case string:tokens(RoomFull, "@") of
						[Room, Host] -> 
							case get_room_pid(Room, Host) of
								unknown -> true;
								Room_pid -> (get_room_config(Room_pid))#config.public
							end;
						_ -> false % Don't show files that are not rooms
					end
				end,
				Listing2);
		_ ->
			Listing2
	end,

	Listing4 = lists:map(
			fun(RoomFull) ->
				Desc = case string:tokens(RoomFull, "@") of
					[Room, Host] -> 
						case get_room_pid(Room, Host) of
							unknown -> "-";
							Room_pid -> (get_room_config(Room_pid))#config.title
						end;
					_ -> "-"
				end,
				{RoomFull, Desc}
			end,
			Listing3),

	DirNest = ["Chatroom logs" | LocalPath],
	{_, Indexof1} = lists:foldl(
		fun(E, {N, Res}) ->
			D = string:copies("../", N),
			Res2 = Res 
				++ io_lib:format("<a href='~s'>~s</a>", [D, E])
				++ " &gt; ",
			{N-1, Res2}
		end,
		{length(DirNest)-1, ""},
		DirNest),

	Title1 = lists:foldl(
		fun(E, Res) ->
			Res ++ E ++ " &gt; "
		end,
		"",
		DirNest),

	Title = io_lib:format("<title>~s</title>", [Title1]),
	Indexof = io_lib:format("<h1>~s</h1>", [Indexof1]),

	Files = lists:foldl(
		fun({Filename, Description}, Res) ->
			{ok, Fi} = file:read_file_info(DirName ++ "/" ++ Filename),

			{Filename2, Size} = case Fi#file_info.type of
				directory -> {Filename ++ "/", "-"};
				_ -> {Filename, integer_to_list(Fi#file_info.size)}
			end,

			DateTimeList = build_datetimelist(Fi#file_info.ctime),
			Time = io_lib:format("~s-~s-~s ~s:~s:~s", DateTimeList),

			FillSpace = lists:flatten(lists:duplicate(50-length(Filename2), " ")),

			FileString = io_lib:format("<a href='~s'>~s</a>~s ~s ~10s ~s~n", 
				[Filename2, Filename2, FillSpace, Time, Size, Description]),
			Res ++ FileString
		end,
		"",
		Listing4),

	Content = "<html><head>" 
		++ Title 
		++ "</head><body><pre>"
		++ Indexof
		++ Header
		++ "<hr>"
		++ Files
		++ "<hr></pre>"
		++ Address
		++ "</body></html>",

	{200,
	 [{"Server", "ejabberd"},
	  {"Content-Type", "text/html"}],
	 Content}.


%%%----------------------------------------------------------------------
%%% UTILITIES
%%%----------------------------------------------------------------------

content_type(Filename) ->
	case ?STRING2LOWER:to_lower(filename:extension(Filename)) of
		".jpg"  -> "image/jpeg";
		".jpeg" -> "image/jpeg";
		".gif"  -> "image/gif";
		".png"  -> "image/png";
		".html" -> "text/html";
		".css"  -> "text/css";
		".txt"  -> "text/plain";
		".xul"  -> "application/vnd.mozilla.xul+xml";
		".jar"  -> "application/java-archive";
		".xpi"  -> "application/x-xpinstall";
		_Else   -> "application/octet-stream"
	end.

last_modified(FileName) ->
    {ok, FileInfo} = file:read_file_info(FileName),
    Then = FileInfo#file_info.mtime,
    httpd_util:rfc1123_date(Then).

loop(DocRoot) ->
	receive
		{get_docroot, Pid} ->
			Pid ! DocRoot,
			loop(DocRoot);
		stop ->
			ok
	end.

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------

start(Host, _Opts) -> 
	DocRoot = gen_mod:get_module_opt(Host, mod_muc_log, outdir, "www/muc"),
	Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	catch register(Proc, spawn(?MODULE, loop, [DocRoot])),
	ok.

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    exit(whereis(Proc), stop),
    {wait, Proc}.
