%%%----------------------------------------------------------------------
%%% File    : mod_log_chat.erl
%%% Author  : Jérôme Sautret <jerome.sautret@process-one.net>
%%% Purpose : Log 2 ways chat messages in files
%%% Id      : $Id: mod_log_chat.erl 412 2007-11-15 10:10:09Z mremond $
%%%----------------------------------------------------------------------

-module(mod_log_chat).
-author('jerome.sautret@process-one.net').

-behaviour(gen_mod).

-export([start/2,
         init/1,
	 stop/1,
	 log_packet_send/3,
	 log_packet_receive/4]).

%-define(ejabberd_debug, true).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(PROCNAME, ?MODULE).
-define(DEFAULT_PATH, ".").
-define(DEFAULT_FORMAT, text).

-record(config, {path=?DEFAULT_PATH, format=?DEFAULT_FORMAT}).

start(Host, Opts) ->
    ?DEBUG(" ~p  ~p~n", [Host, Opts]),
    case gen_mod:get_opt(host_config, Opts, []) of
	[] ->
	    start_vh(Host, Opts);
	HostConfig ->
	    start_vhs(Host, HostConfig)
    end.

start_vhs(_, []) ->
    ok;
start_vhs(Host, [{Host, Opts}| Tail]) ->
    ?DEBUG("start_vhs ~p  ~p~n", [Host, [{Host, Opts}| Tail]]),
    start_vh(Host, Opts),
    start_vhs(Host, Tail);
start_vhs(Host, [{_VHost, _Opts}| Tail]) ->
    ?DEBUG("start_vhs ~p  ~p~n", [Host, [{_VHost, _Opts}| Tail]]),
    start_vhs(Host, Tail).
start_vh(Host, Opts) ->
    Path = gen_mod:get_opt(path, Opts, ?DEFAULT_PATH),
    Format = gen_mod:get_opt(format, Opts, ?DEFAULT_FORMAT),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, log_packet_send, 55),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, log_packet_receive, 55),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, init, [#config{path=Path, format=Format}])).

init(Config)->
    ?DEBUG("Starting ~p with config ~p~n", [?MODULE, Config]),
    loop(Config).

loop(Config) ->
    receive
	{call, Caller, get_config} ->
	    Caller ! {config, Config},
	    loop(Config);
	stop ->
	    exit(normal)
    end.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host,
			  ?MODULE, log_packet_send, 55),
    ejabberd_hooks:delete(user_receive_packet, Host,
			  ?MODULE, log_packet_receive, 55),
    gen_mod:get_module_proc(Host, ?PROCNAME) ! stop,
    ok.

log_packet_send(From, To, Packet) ->
    log_packet(From, To, Packet, From#jid.lserver).

log_packet_receive(_JID, From, To, _Packet) when From#jid.lserver == To#jid.lserver->
    ok; % only log at send time if the message is local to the server
log_packet_receive(_JID, From, To, Packet) ->
    log_packet(From, To, Packet, To#jid.lserver).

log_packet(From, To, Packet = {xmlelement, "message", Attrs, _Els}, Host) ->
    case xml:get_attr_s("type", Attrs) of
	"groupchat" -> %% mod_muc_log already does it
	    ?DEBUG("dropping groupchat: ~s", [xml:element_to_string(Packet)]),
	    ok;
	"error" -> %% we don't log errors
	    ?DEBUG("dropping error: ~s", [xml:element_to_string(Packet)]),
	    ok;
	_ ->
	    write_packet(From, To, Packet, Host)
    end;
log_packet(_From, _To, _Packet, _Host) ->
    ok.

write_packet(From, To, Packet, Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME) ! {call, self(), get_config},
    Config = receive
	       {config, Result} ->
		   Result
	   end,
    Format = Config#config.format,
    {Subject, Body} = {case xml:get_path_s(Packet, [{elem, "subject"}, cdata]) of
			   false ->
			       "";
			   Text ->
			       escape(Format, Text)
		       end,
		       escape(Format, xml:get_path_s(Packet, [{elem, "body"}, cdata]))},
    case Subject ++ Body of
        "" -> %% don't log empty messages
            ?DEBUG("not logging empty message from ~s",[jlib:jid_to_string(From)]),
            ok;
        _ ->
	    Path = Config#config.path,
	    FromJid = From#jid.luser++"@"++From#jid.lserver,
	    ToJid = To#jid.luser++"@"++To#jid.lserver,
	    {FilenameTemplate, DateString, Header, MessageType} =
		case calendar:local_time() of
		    {{Y, M, D}, {H, Min, S}} ->
			SortedJid = lists:sort([FromJid, ToJid]),
			Title = io_lib:format(template(Format, title), [FromJid, ToJid, Y, M, D]),
			{lists:flatten(io_lib:format("~s/~~p-~~2.2.0w-~~2.2.0w ~s - ~s~s",
				       [Path | SortedJid]++[template(Format, extension)])),
			 io_lib:format(template(Format, date), [Y, M, D, H, Min, S]),

			 io_lib:format(template(Format, header),
				       lists:duplicate(count(template(Format, header), "~s"),
						       Title)
				      ),
			 case hd(SortedJid) of
			     FromJid ->
				 message1;
			     ToJid ->
				 message2
			 end
			}
		end,
	    ?DEBUG("FilenameTemplate ~p~n",[FilenameTemplate]),
	    Filename = io_lib:format(FilenameTemplate, [Y, M, D]),
	    ?DEBUG("logging message from ~s into ~s~n",[jlib:jid_to_string(From), Filename]),
	    File = case file:read_file_info(Filename) of
		       {ok, _} ->
			   open_logfile(Filename);
		       {error, enoent} ->
			   close_previous_logfile(FilenameTemplate, Format, {Y, M, D}),
			   NewFile = open_logfile(Filename),
			   io:format(NewFile, Header, []),
			   NewFile
		   end,
	    MessageText = case Subject of
		       "" ->
			   Body;
		       _ ->
			   io_lib:format(template(Format, subject), [Subject])++Body
		   end,
	    ?DEBUG("MessageTemplate ~s~n",[template(Format, MessageType)]),
	    io:format(File, lists:flatten(template(Format, MessageType)), [DateString, FromJid, From#jid.lresource, ToJid,
							    To#jid.lresource, MessageText]),
	    file:close(File)
    end.

open_logfile(Filename) ->
    case file:open(Filename, [append]) of
	{ok, File} ->
	    File;
	{error, Reason} ->
	    ?ERROR_MSG("Cannot write into file ~s: ~p~n", [Filename, Reason])
    end.

close_previous_logfile(FilenameTemplate, Format, Date) ->
    Yesterday = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - 1),
    Filename = io_lib:format(FilenameTemplate, tuple_to_list(Yesterday)),
    case file:read_file_info(Filename) of
	{ok, _} ->
	    File = open_logfile(Filename),
	    io:format(File, template(Format, footer), []),
	    file:close(File);
	{error, enoent} ->
	    ok
    end.

escape(text, Text) ->
    Text;
escape(_, "") ->
    "";
escape(html, [$< | Text]) ->
    "&lt;" ++ escape(html, Text);
escape(html, [$& | Text]) ->
    "&amp;" ++ escape(html, Text);
escape(html, [Char | Text]) ->
    [Char | escape(html, Text)].


% return the number of occurence of Word in String
count(String, Word) ->
    case string:str(String, Word) of
	0 ->
	    0;
	N ->
	    1+count(string:substr(String, N+length(Word)), Word)
    end.



template(text, extension) ->
    ".log";
template(text, title) ->
    "Messages log between ~s and ~s on ~p-~2.2.0w-~2.2.0w";
template(text, header) ->
    "~s~n-----------------------------------------------------------------------~n";
template(text, subject) ->
    "Subject: ~s~n";
template(text, message) ->
    "~~s ~~s/~~s -> ~~s/~~s~n~s~~s~n";
template(text, message1) ->
    io_lib:format(template(text, message), ["> "]);
template(text, message2) ->
    io_lib:format(template(text, message), ["< "]);
template(text, date) ->
    "~p-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w";
template(text, footer) ->
    "---- End ----~n";

template(html, extension) ->
    ".html";
template(html, title) ->
    template(text, title);
template(html, header) ->
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">~n"++
	"<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><title>~s</title>"++
	css()++
	"</head><body>~n<h1>~s</h1>~n";
template(html, subject) ->
    "<div class=\"subject\"><span>Subject:</span> ~s</div>";
template(html, message) ->
    "<div class=\"message~w\"><span class=\"date\">~~s</span> <span class=\"jid\">~~s</span>/<span class=\"ressource\">~~s</span> -&gt; <span class=\"jid\">~~s</span>/<span class=\"ressource\">~~s</span>~~n<span class=\"messagetext\">~~s</span></div>~~n";
template(html, message1) ->
    io_lib:format(template(html, message), [1]);
template(html, message2) ->
    io_lib:format(template(html, message), [2]);
template(html, date) ->
    "~p-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w";
template(html, footer) ->
    "</body></html>".

css() ->
    "<style type=\"text/css\">~n<!--~n"++
	"h1 {border-bottom: #224466 solid 3pt; margin-left: 20pt; color: #336699; font-size: 1.5em; font-weight: bold; font-family: sans-serif; letter-spacing: 0.1empx; text-decoration: none;}~n"++
	".message1 {border: black solid 1pt; background-color: #d7e4f1; margin: 0.3em}~n"++
	".message2 {border: black solid 1pt; background-color: #b39ccb; margin: 0.3em}~n"++
	".subject {margin-left: 0.5em}~n"++
	".subject span {font-weight: bold;}~n"++
	".date {color: #663399; font-size: 0.8em; font-weight: bold; font-family: sans-serif; letter-spacing: 0.05em; margin-left:0.5em; margin-top:20px;}~n"++
	".jid {color: #336699; font-weight: bold; font-size: 1em; }~n"++
	".ressource {color: #336699; }~n"++
	".messagetext {color: black; margin: 0.2em; clear: both; display: block;}~n"++
	"//-->~n</style>~n".
