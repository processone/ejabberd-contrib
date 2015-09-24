-module(ejabberd_ircd).
-author('henoch@dtek.chalmers.se').
-update_info({update, 0}).

-behaviour(gen_fsm).

%% External exports
-export([start/2,
	 start_link/2,
	 socket_type/0]).

%% gen_fsm callbacks
-export([init/1,
	 wait_for_nick/2,
	 wait_for_cmd/2,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3
	]).

%-define(ejabberd_debug, true).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-define(DICT, dict).

-record(state, {socket,
		sockmod,
		access,
		encoding,
		shaper,
		host,
		muc_host,
		sid = none,
		pass = "",
		nick = none,
		user = none,
		%% joining is a mapping from room JIDs to nicknames
		%% received but not yet forwarded
		joining = ?DICT:new(),
		joined = ?DICT:new(),
		%% mapping certain channels to certain rooms
		channels_to_jids = ?DICT:new(),
		jids_to_channels = ?DICT:new()
	       }).
-record(channel, {participants = [],
		  topic = ""}).

-record(line, {prefix, command, params}).

%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    supervisor:start_child(ejabberd_ircd_sup, [SockData, Opts]).

start_link(SockData, Opts) ->
    gen_fsm:start_link(ejabberd_ircd, [SockData, Opts], ?FSMOPTS).

socket_type() ->
    raw.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%----------------------------------------------------------------------
init([{SockMod, Socket}, Opts]) ->
    %iconv:start(),
    Access = case lists:keysearch(access, 1, Opts) of
		 {value, {_, A}} -> A;
		 _ -> all
	     end,
    Shaper = case lists:keysearch(shaper, 1, Opts) of
		 {value, {_, S}} -> S;
		 _ -> none
	     end,
    Host = case lists:keysearch(host, 1, Opts) of
	       {value, {_, H}} -> H;
	       _ -> ?MYNAME
	   end,
    MucHost = case lists:keysearch(muc_host, 1, Opts) of
		  {value, {_, M}} -> M;
		  _ -> "conference." ++ ?MYNAME
	      end,
    Encoding = case lists:keysearch(encoding, 1, Opts) of
		   {value, {_, E}} -> E;
		   _ -> "utf-8"
	       end,
    ChannelMappings = case lists:keysearch(mappings, 1, Opts) of
			  {value, {_, C}} -> C;
			  _ -> []
		      end,
    {ChannelToJid, JidToChannel} =
	lists:foldl(fun({Channel, Room}, {CToJ, JToC}) ->
			    RoomJID = jlib:string_to_jid(Room),
			    BareChannel = case Channel of
					      [$#|R] -> R;
					      _ -> Channel
					  end,
			    {?DICT:store(BareChannel, RoomJID, CToJ),
			     ?DICT:store(RoomJID, BareChannel, JToC)}
		    end, {?DICT:new(), ?DICT:new()},
		    ChannelMappings),
    inet:setopts(Socket, [list, {packet, line}, {active, true}]),
    %%_ReceiverPid = start_ircd_receiver(Socket, SockMod),
    {ok, wait_for_nick, #state{socket    = Socket,
			       sockmod   = SockMod,
			       access    = Access,
			       encoding  = Encoding,
			       shaper    = Shaper,
			       host      = Host,
			       muc_host  = MucHost,
			       channels_to_jids = ChannelToJid,
			       jids_to_channels = JidToChannel
			      }}.

handle_info({tcp, _Socket, Line}, StateName, StateData) ->
    %DecodedLine = iconv:convert(StateData#state.encoding, "utf-8", Line),
    DecodedLine = Line,
    Parsed = parse_line(DecodedLine),
    ?MODULE:StateName({line, Parsed}, StateData);
handle_info({tcp_closed, _}, _StateName, StateData) ->
    {stop, normal, StateData};
handle_info({route, _, _, _} = Event, StateName, StateData) ->
    ?MODULE:StateName(Event, StateData);
handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {next_state, StateName, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    ?ERROR_MSG("Unexpected sync event: ~p", [Event]),
    Reply = ok,
    {reply, Reply, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
terminate(_Reason, _StateName, #state{socket = Socket, sockmod = SockMod,
				      sid = SID, host = Host, nick = Nick,
				      joined = JoinedDict} = State) ->
    ?INFO_MSG("closing IRC connection for ~p", [Nick]),
    case SID of
	none ->
	    ok;
	_ ->
	    Packet = {xmlel, <<"presence">>,
		      [{<<"type">>, <<"unavailable">>}], []},
	    FromJID = user_jid(State),
	    ?DICT:map(fun(ChannelJID, _ChannelData) ->
			      ejabberd_router:route(FromJID, ChannelJID, Packet)
		      end, JoinedDict),
	    ejabberd_sm:close_session_unset_presence(SID, Nick, Host, "irc", "Logged out")
    end,
    gen_tcp = SockMod,
    ok = gen_tcp:close(Socket),
    ok.


wait_for_nick({line, #line{command = "PASS", params = Params}}, State) ->
    ?DEBUG("in wait_for_nick", []),
    Pass = hd(Params),
    ?DEBUG("got password", []),
    {next_state, wait_for_nick, State#state{pass = Pass}};
wait_for_nick({line, #line{command = "NICK", params = Params}}, State) ->
    ?DEBUG("in wait_for_nick", []),
    Nick = hd(Params),
    Pass = State#state.pass,
    Server = State#state.host,
    ?DEBUG("user=~p server=~p", [Nick, Server]),

    JID = jlib:make_jid(list_to_binary(Nick), Server, <<"irc">>),
    ?DEBUG("JID=~p", [JID]),
    case JID of
	error ->
	    ?DEBUG("invalid nick '~p'", [Nick]),
	    send_reply('ERR_ERRONEUSNICKNAME', [Nick, "Erroneous nickname"], State),
	    {next_state, wait_for_nick, State};
	_ ->
	    case acl:match_rule(Server, State#state.access, JID) of
		deny ->
		    ?DEBUG("access denied for '~p'", [Nick]),
		    send_reply('ERR_NICKCOLLISION', [Nick, "Nickname collision"], State),
		    {next_state, wait_for_nick, State};
		allow ->
		    case ejabberd_auth:check_password(list_to_binary(Nick), Server, list_to_binary(Pass)) of
			false ->
			    ?DEBUG("auth failed for '~p'", [Nick]),
			    send_reply('ERR_NICKCOLLISION', [Nick, "Authentication failed"], State),
			    {next_state, wait_for_nick, State};
			true ->
			    ?DEBUG("good nickname '~p'", [Nick]),
			    SID = {now(), self()},
			    ejabberd_sm:open_session(
			      SID, list_to_binary(Nick), Server, <<"irc">>, peerip(gen_tcp, State#state.socket)),
			    ejabberd_sm:set_presence(SID, list_to_binary(Nick), Server, <<"irc">>,
						     3, "undefined",
						     [{'ip', peerip(gen_tcp, State#state.socket)}, {'conn','c2s'}, {'state',"+"}]),
			    send_text_command("", "001", [Nick, "IRC interface of ejabberd server "++Server], State),
			    send_reply('RPL_MOTDSTART', [Nick, "- "++binary_to_list(Server)++" Message of the day - "], State),
			    send_reply('RPL_MOTD', [Nick, "- This is the IRC interface of the ejabberd server "++binary_to_list(Server)++"."], State),
			    send_reply('RPL_MOTD', [Nick, "- Your full JID is "++Nick++"@"++binary_to_list(Server)++"/irc."], State),
			    send_reply('RPL_MOTD', [Nick, "- Channel #whatever corresponds to MUC room whatever@"++binary_to_list(State#state.muc_host)++"."], State),
			    send_reply('RPL_MOTD', [Nick, "- This IRC interface is quite immature.  You will probably find bugs."], State),
			    send_reply('RPL_MOTD', [Nick, "- Have a good time!"], State),
			    send_reply('RPL_ENDOFMOTD', [Nick, "End of /MOTD command"], State),
			    {next_state, wait_for_cmd, State#state{nick = Nick, sid = SID, pass = ""}}
		    end
	    end
    end;
wait_for_nick(Event, State) ->
    ?DEBUG("in wait_for_nick", []),
    ?INFO_MSG("unexpected event ~p", [Event]),
    {next_state, wait_for_nick, State}.

peerip(SockMod, Socket) ->
    IP = case SockMod of
	     gen_tcp -> inet:peername(Socket);
	     _ -> SockMod:peername(Socket)
	 end,
    case IP of
	{ok, IPOK} -> IPOK;
	_ -> undefined
    end.

wait_for_cmd({line, #line{command = "USER", params = [_Username, _Hostname, _Servername, _Realname]}}, State) ->
    %% Yeah, like we care.
    {next_state, wait_for_cmd, State};
wait_for_cmd({line, #line{command = "JOIN", params = Params}}, State) ->
    ?DEBUG("received JOIN ~p", [Params]),
    {ChannelsString, KeysString} =
	case Params of
	    [C, K] ->
		{C, K};
	    [C] ->
		{C, []}
	end,
    Channels = string:tokens(ChannelsString, ","),
    Keys = string:tokens(KeysString, ","),
    ?DEBUG("joining channels ~p", [Channels]),
    NewState = join_channels(Channels, Keys, State),
    ?DEBUG("joined channels ~p", [Channels]),
    {next_state, wait_for_cmd, NewState};

%% USERHOST command
wait_for_cmd({line, #line{command = "USERHOST", params = Params}}, State) ->
    case Params of
        [] ->
	    send_reply('ERR_NEEDMOREPARAMS', ["USERHOST", "Not enough parameters"], State);
        UserParams ->
	    Users = lists:sublist(string:tokens(UserParams, " "), 5), %% RFC 1459 specifies 5 items max
	    lists:foreach(
	      fun(UserSubList) ->
		      User = lists:last(UserSubList),
		      case ejabberd_sm:get_user_info(User, State#state.host, "irc") of
			  offline ->
			      send_reply('RPL_USERHOST',[State#state.nick, User++" offline"], State);
			  [_Node, _Conn, Ip] ->
			      {_,{{IP1,IP2,IP3,IP4}, _}} = Ip,
			      send_reply('RPL_USERHOST',[State#state.nick, User ++ "=+" ++ integer_to_list(IP1) ++ "." ++
							 integer_to_list(IP2) ++ "." ++ integer_to_list(IP3) ++ "." ++ integer_to_list(IP4)], State)
		      end
	      end, Users)
    end,
    {next_state, wait_for_cmd, State};

wait_for_cmd({line, #line{command = "PART", params = [ChannelsString | MaybeMessage]}}, State) ->
    Message = case MaybeMessage of
		  [] -> nothing;
		  [M] -> M
	      end,
    Channels = string:tokens(ChannelsString, ","),
    NewState = part_channels(Channels, State, Message),
    {next_state, wait_for_cmd, NewState};

wait_for_cmd({line, #line{command = "PRIVMSG", params = [To, Text]}}, State) ->
    Recipients = string:tokens(To, ","),
    FromJID = user_jid(State),
    lists:foreach(
      fun(Rcpt) ->
	      case Rcpt of
		  [$# | Roomname] ->
		      Packet = {xmlel, <<"message">>,
				[{<<"type">>, <<"groupchat">>}],
				[{xmlel, <<"body">>, [],
				  filter_cdata(translate_action(Text))}]},
		      ToJID = channel_to_jid(Roomname, State),
		      ejabberd_router:route(FromJID, ToJID, Packet);
		  _ ->
		      case string:tokens(Rcpt, "#") of
			  [Nick, Channel] ->
			      Packet = {xmlel, <<"message">>,
					[{<<"type">>, <<"chat">>}],
					[{xmlel, <<"body">>, [],
					  filter_cdata(translate_action(Text))}]},
			      ToJID = channel_nick_to_jid(Nick, Channel, State),
			      ejabberd_router:route(FromJID, ToJID, Packet);
			  _ ->
			      send_text_command(Rcpt, "NOTICE", [State#state.nick,
								 "Your message to "++
								 Rcpt++
								 " was dropped.  "
								 "Try sending it to "++Rcpt++
								 "#somechannel."], State)
		      end
	      end
      end, Recipients),
    {next_state, wait_for_cmd, State};

wait_for_cmd({line, #line{command = "PING", params = Params}}, State) ->
    {Token, Whom} =
	case Params of
	    [A] ->
		{A, ""};
	    [A, B] ->
		{A, B}
	end,
    if Whom == ""; Whom == State#state.host ->
	    %% Ping to us
	    send_command("", "PONG", [State#state.host, Token], State);
       true ->
	    %% Ping to someone else
	    ?DEBUG("ignoring ping to ~s", [Whom]),
	    ok
    end,
    {next_state, wait_for_cmd, State};

wait_for_cmd({line, #line{command = "TOPIC", params = Params}}, State) ->
    case Params of
	[Channel] ->
	    %% user asks for topic
	    case ?DICT:find(channel_to_jid(Channel, State),
			    State#state.joined) of
		{ok, #channel{topic = Topic}} ->
		    case Topic of
			"" ->
			    send_reply('RPL_NOTOPIC', ["No topic is set"], State);
			_ ->
			    send_reply('RPL_TOPIC', [Topic], State)
		    end;
		_ ->
		    send_reply('ERR_NOTONCHANNEL', ["You're not on that channel"], State)
	    end;
	[Channel, NewTopic] ->
	    Packet =
		{xmlel, <<"message">>,
		 [{<<"type">>, <<"groupchat">>}],
		 [{xmlel, <<"subject">>, [], filter_cdata(NewTopic)}]},
	    FromJID = user_jid(State),
	    ToJID = channel_to_jid(Channel, State),
	    ejabberd_router:route(FromJID, ToJID, Packet)
    end,
    {next_state, wait_for_cmd, State};

wait_for_cmd({line, #line{command = "MODE", params = [ModeOf | Params]}}, State) ->
    case ModeOf of
	[$# | Channel] ->
	    ChannelJid = channel_to_jid(Channel, State),
	    Joined = ?DICT:find(ChannelJid, State#state.joined),
	    case Joined of
		{ok, _ChannelData} ->
		    case Params of
			[] ->
			    %% This is where we could mirror some advanced MUC
			    %% properties.
			    %%send_reply('RPL_CHANNELMODEIS', [Channel, Modes], State);
			    send_reply('ERR_NOCHANMODES', [Channel], State);
			["b"] ->
			    send_reply('RPL_ENDOFBANLIST', [Channel, "Ban list not available"], State);
			_ ->
			    send_reply('ERR_UNKNOWNCOMMAND', ["MODE", io_lib:format("MODE ~p not understood", [Params])], State)
		    end;
		_ ->
		    send_reply('ERR_NOTONCHANNEL', [Channel, "You're not on that channel"], State)
	    end;
	Nick ->
	    if Nick == State#state.nick ->
		    case Params of
			[] ->
			    send_reply('RPL_UMODEIS', [], State);
			[Flags|_] ->
			    send_reply('ERR_UMODEUNKNOWNFLAG', [Flags, "No MODE flags supported"], State)
		    end;
	       true ->
		    send_reply('ERR_USERSDONTMATCH', ["Can't change mode for other users"], State)
	    end
    end,
    {next_state, wait_for_cmd, State};

wait_for_cmd({line, #line{command = "QUIT"}}, State) ->
    %% quit message is ignored for now
    {stop, normal, State};

wait_for_cmd({line, #line{command = Unknown, params = Params} = Line}, State) ->
    ?INFO_MSG("Unknown command: ~p", [Line]),
    send_reply('ERR_UNKNOWNCOMMAND', [Unknown, "Unknown command or arity: " ++
				      Unknown ++ "/" ++ integer_to_list(length(Params))], State),
    {next_state, wait_for_cmd, State};

wait_for_cmd({route, From, _To, {xmlel, <<"presence">>, Attrs, Els} = El}, State) ->
    ?DEBUG("Received a Presence ~p ~p ~p", [From, _To, El]),
    Type = xml:get_attr_s("type", Attrs),
    FromRoom = jlib:jid_remove_resource(From),
    FromNick = binary_to_list(From#jid.resource),

    Channel = jid_to_channel(From, State),
    MyNick = State#state.nick,
    IRCSender = make_irc_sender(FromNick, FromRoom, State),

    Joining = ?DICT:find(FromRoom, State#state.joining),
    Joined = ?DICT:find(FromRoom, State#state.joined),
    ?DEBUG("JoinState ~p ~p ~p", [Joining, Joined, Type]),
    case {Joining, Joined, Type} of
	{{ok, BufferedNicks}, _, <<"">>} ->
            ?DEBUG("BufferedNicks ~p", [BufferedNicks]),
	    case BufferedNicks of
		[] ->
		    %% If this is the first presence, tell the
		    %% client that it's joining.
                    ?DEBUG("Sending Command ~p ~p", [IRCSender, Channel]),
		    send_command(IRCSender, "JOIN", [Channel], State),
                    ?DEBUG("Command Sent", []);
		_ ->
		    ok
	    end,

            ?DEBUG("Getting NewRole", []),
	    NewRole = case find_el("x", ?NS_MUC_USER, Els) of
			  nothing ->
			      "";
			  XMucEl ->
			      xml:get_path_s(XMucEl, [{elem, "item"}, {attr, "role"}])
		      end,
            ?DEBUG("NewRole ~p", [NewRole]),
	    NewBufferedNicks = [{FromNick, NewRole} | BufferedNicks],
	    ?DEBUG("~s is present in ~s.  we now have ~p.",
		   [FromNick, Channel, NewBufferedNicks]),
	    %% We receive our own presence last.  XXX: there
	    %% are some status codes here.  See XEP-0045,
	    %% section 7.1.3.
	    NewState =
		case FromNick of
		    MyNick ->
			send_reply('RPL_NAMREPLY',
				   [MyNick, "=",
				    Channel,
				    lists:append(
				      lists:map(
					fun({Nick, Role}) ->
						case Role of
						    "moderator" ->
							"@";
						    "participant" ->
							"+";
						    _ ->
							""
						end ++ Nick ++ " "
					end, NewBufferedNicks))],
				   State),
			send_reply('RPL_ENDOFNAMES',
				   [Channel,
				    "End of /NAMES list"],
				   State),
			NewJoiningDict = ?DICT:erase(FromRoom, State#state.joining),
			ChannelData = #channel{participants = NewBufferedNicks},
			NewJoinedDict = ?DICT:store(FromRoom, ChannelData, State#state.joined),
			State#state{joining = NewJoiningDict,
				    joined = NewJoinedDict};
		    _ ->
			NewJoining = ?DICT:store(FromRoom, NewBufferedNicks, State#state.joining),
			State#state{joining = NewJoining}
		end,
	    {next_state, wait_for_cmd, NewState};
	{{ok, _BufferedNicks}, _, <<"error">>} ->
	    NewState =
		case FromNick of
		    MyNick ->
			%% we couldn't join the room
			{ReplyCode, ErrorDescription} =
			    case xml:get_subtag(El, "error") of
				{xmlel, _, _, _} = ErrorEl ->
				    {ErrorName, ErrorText} = parse_error(ErrorEl),
				    {case ErrorName of
					 "forbidden" -> 'ERR_INVITEONLYCHAN';
					 _ -> 'ERR_NOSUCHCHANNEL'
				     end,
				     if is_list(ErrorText) ->
					     ErrorName ++ ": " ++ ErrorText;
					true ->
					     ErrorName
				     end};
				_ ->
				    {'ERR_NOSUCHCHANNEL', "Unknown error"}
			    end,
			send_reply(ReplyCode, [Channel, ErrorDescription], State),

			NewJoiningDict = ?DICT:erase(FromRoom, State#state.joining),
			State#state{joining = NewJoiningDict};
		    _ ->
			?ERROR_MSG("ignoring presence of type ~s from ~s while joining room",
				   [Type, jlib:jid_to_string(From)]),
			State
		end,
	    {next_state, wait_for_cmd, NewState};
	%% Presence in a channel we have already joined
	{_, {ok, _}, <<"">>} ->
	    %% Someone enters
	    send_command(IRCSender, "JOIN", [Channel], State),
	    {next_state, wait_for_cmd, State};
	{_, {ok, _}, _} ->
	    %% Someone leaves
	    send_command(IRCSender, "PART", [Channel], State),
	    {next_state, wait_for_cmd, State};
	_ ->
	    ?INFO_MSG("unexpected presence from ~s", [jlib:jid_to_string(From)]),
	    {next_state, wait_for_cmd, State}
    end;

wait_for_cmd({route, From, _To, {xmlel, <<"message">>, Attrs, Els} = El}, State) ->
    ?DEBUG("Got a Message! ~p ~p ~p", [From, _To, El]),
    Type = xml:get_attr_s(<<"type">>, Attrs),
    case Type of
	<<"groupchat">> ->
            ?DEBUG("It's a groupchat", []),
	    ChannelJID = jlib:jid_remove_resource(From),
	    case ?DICT:find(ChannelJID, State#state.joined) of
		{ok, #channel{} = ChannelData} ->
		    FromChannel = jid_to_channel(From, State),
		    FromNick = binary_to_list(From#jid.resource),
		    Subject = xml:get_path_s(El, [{elem, <<"subject">>}, cdata]),
		    Body = xml:get_path_s(El, [{elem, <<"body">>}, cdata]),
                    ?DEBUG("Message Data ~p ~p", [Subject, Body]),
		    XDelay = lists:any(fun({xmlel, <<"x">>, XAttrs, _}) ->
					       xml:get_attr_s(<<"xmlns">>, XAttrs) == ?NS_DELAY;
					  (_) ->
					       false
				       end, Els),
                    ?DEBUG("XDelay ~p", [XDelay]),
		    if
			Subject /= <<"">> ->
                            ?DEBUG("Cleaning Subject!", []),
			    CleanSubject = lists:map(fun($\n) ->
							     $\ ;
							(C) -> C
						     end, binary_to_list(Subject)),
                            ?DEBUG("CleanSubject ~p", [CleanSubject]),
                            IRCSender = make_irc_sender(From, State),
                            ?DEBUG("IRCSender ~p", [IRCSender]),
			    send_text_command(IRCSender,
					      "TOPIC", [FromChannel, CleanSubject], State),
			    NewChannelData = ChannelData#channel{topic = CleanSubject},
			    NewState = State#state{joined = ?DICT:store(jlib:jid_remove_resource(From), NewChannelData, State#state.joined)},
			    {next_state, wait_for_cmd, NewState};
			not XDelay, FromNick == State#state.nick ->
			    %% there is no message echo in IRC.
			    %% we let the backlog through, though.
                            ?DEBUG("Don't care about it", []),
			    {next_state, wait_for_cmd, State};
			true ->
                            ?DEBUG("Send it to someone!", []),
			    BodyLines = string:tokens(binary_to_list(Body), "\n"),
			    lists:foreach(
			      fun(Line) ->
				      Line1 =
					  case Line of
					      [$/, $m, $e, $  | Action] ->
						  [1]++"ACTION "++Action++[1];
					      _ ->
						  Line
					  end,
				      send_text_command(make_irc_sender(From, State),
							"PRIVMSG", [FromChannel, Line1], State)
			      end, BodyLines),
			    {next_state, wait_for_cmd, State}
		    end;
		error ->
		    ?ERROR_MSG("got message from ~s without having joined it",
			       [jlib:jid_to_string(ChannelJID)]),
		    {next_state, wait_for_cmd, State}
	    end;
	<<"error">> ->
	    MucHost = State#state.muc_host,
	    ErrorFrom =
		case From of
		    #jid{lserver = MucHost,
			 luser = Room,
			 lresource = ""} ->
			[$#|Room];
		    #jid{lserver = MucHost,
			 luser = Room,
			 lresource = Nick} ->
			Nick++"#"++Room;
		    #jid{} ->
			%% ???
			jlib:jid_to_string(From)
		end,
	    %% I think this should cover all possible combinations of
	    %% XMPP and non-XMPP error messages...
	    ErrorText =
		error_to_string(xml:get_subtag(El, <<"error">>)),
	    send_text_command("", "NOTICE", [State#state.nick,
					     "Message to "++ErrorFrom++" bounced: "++
					     ErrorText], State),
	    {next_state, wait_for_cmd, State};
	_ ->
	    ChannelJID = jlib:jid_remove_resource(From),
	    case ?DICT:find(ChannelJID, State#state.joined) of
		{ok, #channel{}} ->
		    FromNick = binary_to_list(From#jid.lresource)++jid_to_channel(From, State),
		    Body = xml:get_path_s(El, [{elem, <<"body">>}, cdata]),
		    BodyLines = string:tokens(Body, "\n"),
		    lists:foreach(
		      fun(Line) ->
			      Line1 =
				  case Line of
				      [$/, $m, $e, $  | Action] ->
					  [1]++"ACTION "++Action++[1];
				      _ ->
					  Line
				  end,
			      send_text_command(FromNick, "PRIVMSG", [State#state.nick, Line1], State)
		      end, BodyLines),
		    {next_state, wait_for_cmd, State};
	       _ ->
		    ?INFO_MSG("unexpected message from ~s", [jlib:jid_to_string(From)]),
		    {next_state, wait_for_cmd, State}
	    end
    end;

wait_for_cmd(Event, State) ->
    ?INFO_MSG("unexpected event ~p", [Event]),
    {next_state, wait_for_cmd, State}.

join_channels([], _, State) ->
    State;
join_channels(Channels, [], State) ->
    join_channels(Channels, [none], State);
join_channels([Channel | Channels], [Key | Keys],
	      #state{nick = Nick} = State) ->
    Packet =
	{xmlel, <<"presence">>, [],
	 [{xmlel, <<"x">>, [{<<"xmlns">>, ?NS_MUC}],
	   case Key of
	       none ->
		   [];
	       _ ->
		   [{xmlel, <<"password">>, [], filter_cdata(Key)}]
	   end}]},
    ?DEBUG("joining channel nick=~p channel=~p state=~p", [Nick, Channel, State]),
    From = user_jid(State),
    ?DEBUG("1 ~p", [From]),
    To = channel_nick_to_jid(Nick, Channel, State),
    ?DEBUG("2 ~p", [To]),
    Room = jlib:jid_remove_resource(To),
    ?DEBUG("3 ~p", [Room]),
    ejabberd_router:route(From, To, Packet),
    ?DEBUG("4", []),
    NewState = State#state{joining = ?DICT:store(Room, [], State#state.joining)},
    ?DEBUG("5 ~p", [NewState]),
    join_channels(Channels, Keys, NewState).

part_channels([], State, _Message) ->
    State;
part_channels([Channel | Channels], State, Message) ->
    Packet =
	{xmlel, <<"presence">>,
	 [{<<"type">>, <<"unavailable">>}],
	 case Message of
	    nothing -> [];
	    _ -> [{xmlel, <<"status">>, [],
		  [{xmlcdata, Message}]}]
	 end},
    From = user_jid(State),
    To = channel_nick_to_jid(State#state.nick, Channel, State),
    ejabberd_router:route(From, To, Packet),
    RoomJID = channel_to_jid(Channel, State),
    NewState = State#state{joined = ?DICT:erase(RoomJID, State#state.joined)},
    part_channels(Channels, NewState, Message).

parse_line(Line) ->
    {Line1, LastParam} =
	case string:str(Line, " :") of
	    0 ->
		{Line, []};
	    Index ->
		{string:substr(Line, 1, Index - 1),
		 [string:substr(Line, Index + 2) -- "\r\n"]}
	end,
    Tokens = string:tokens(Line1, " \r\n"),
    {Prefix, Tokens1} =
	case Line1 of
	    [$: | _] ->
		{hd(Tokens), tl(Tokens)};
	    _ ->
		{none, Tokens}
	end,
    [Command | Params] = Tokens1,
    UCCommand = upcase(Command),
    #line{prefix = Prefix, command = UCCommand, params = Params ++ LastParam}.

upcase([]) ->
    [];
upcase([C|String]) ->
    [if $a =< C, C =< $z ->
	     C - ($a - $A);
	true ->
	     C
     end | upcase(String)].

%% sender

send_line(Line, #state{sockmod = SockMod, socket = Socket, encoding = Encoding}) ->
    ?DEBUG("sending ~s", [Line]),
    gen_tcp = SockMod,
    %EncodedLine = iconv:convert("utf-8", Encoding, Line),
    EncodedLine = Line,
    ok = gen_tcp:send(Socket, [EncodedLine, 13, 10]).

send_command(Sender, Command, Params, State) ->
    send_command(Sender, Command, Params, State, false).

%% Some IRC software require commands with text to have the text
%% quoted, even it's not if not necessary.
send_text_command(Sender, Command, Params, State) ->
    send_command(Sender, Command, Params, State, true).

send_command(Sender, Command, Params, State, AlwaysQuote) ->
    ?DEBUG("SendCommand ~p ~p ~p", [Sender, Command, Params]),
    Prefix = case Sender of
		 "" ->
		     [$: | binary_to_list(State#state.host)];
		 _ ->
		     [$: | Sender]
	     end,
    ParamString = make_param_string(Params, AlwaysQuote),
    send_line(Prefix ++ " " ++ Command ++ ParamString, State).

send_reply(Reply, Params, State) ->
    Number = case Reply of
		 'ERR_UNKNOWNCOMMAND' ->
		     "421";
		 'ERR_ERRONEUSNICKNAME' ->
		     "432";
		 'ERR_NICKCOLLISION' ->
		     "436";
		 'ERR_NOTONCHANNEL' ->
		     "442";
		 'ERR_NOCHANMODES' ->
		     "477";
		 'ERR_UMODEUNKNOWNFLAG' ->
		     "501";
		 'ERR_USERSDONTMATCH' ->
		     "502";
		 'RPL_UMODEIS' ->
		     "221";
		 'RPL_CHANNELMODEIS' ->
		     "324";
		 'RPL_NAMREPLY' ->
		     "353";
		 'RPL_ENDOFNAMES' ->
		     "366";
		 'RPL_BANLIST' ->
		     "367";
		 'RPL_ENDOFBANLIST' ->
		     "368";
		 'RPL_NOTOPIC' ->
		     "331";
		 'RPL_TOPIC' ->
		     "332";
		 'RPL_MOTD' ->
		     "372";
		 'RPL_MOTDSTART' ->
		     "375";
		 'RPL_ENDOFMOTD' ->
		     "376"
	     end,
    send_text_command("", Number, Params, State).

make_param_string([], _) -> "";
make_param_string([LastParam], AlwaysQuote) ->
    case {AlwaysQuote, LastParam, lists:member($\ , LastParam)} of
	{true, _, _} ->
	    " :" ++ LastParam;
	{_, _, true} ->
	    " :" ++ LastParam;
	{_, [$:|_], _} ->
	    " :" ++ LastParam;
	{_, _, _} ->
	    " " ++ LastParam
    end;
make_param_string([Param | Params], AlwaysQuote) ->
    case lists:member($\ , Param) of
	false ->
	    " " ++ Param ++ make_param_string(Params, AlwaysQuote)
    end.

find_el(Name, NS, [{xmlel, N, Attrs, _} = El|Els]) ->
    XMLNS = xml:get_attr_s("xmlns", Attrs),
    case {Name, NS} of
	{N, XMLNS} ->
	    El;
	_ ->
	    find_el(Name, NS, Els)
    end;
find_el(_, _, []) ->
    nothing.

channel_to_jid([$#|Channel], State) ->
    channel_to_jid(Channel, State);
channel_to_jid(Channel, #state{muc_host = MucHost,
			       channels_to_jids = ChannelsToJids}) ->
    case ?DICT:find(Channel, ChannelsToJids) of
	{ok, RoomJID} -> RoomJID;
	_ -> jlib:make_jid(list_to_binary(Channel), MucHost, <<"">>)
    end.

channel_nick_to_jid(Nick, [$#|Channel], State) ->
    channel_nick_to_jid(Nick, Channel, State);
channel_nick_to_jid(Nick, Channel, #state{muc_host = MucHost,
					 channels_to_jids = ChannelsToJids}) ->
    case ?DICT:find(Channel, ChannelsToJids) of
	{ok, RoomJID} -> jlib:jid_replace_resource(RoomJID, list_to_binary(Nick));
	_ -> jlib:make_jid(list_to_binary(Channel), MucHost, list_to_binary(Nick))
    end.

jid_to_channel(#jid{user = Room} = RoomJID,
	       #state{jids_to_channels = JidsToChannels}) ->
    case ?DICT:find(jlib:jid_remove_resource(RoomJID), JidsToChannels) of
	{ok, Channel} -> [$#|binary_to_list(Channel)];
	_ -> [$#|binary_to_list(Room)]
    end.

make_irc_sender(Nick, #jid{luser = Room} = RoomJID,
		#state{jids_to_channels = JidsToChannels}) ->
    case ?DICT:find(jlib:jid_remove_resource(RoomJID), JidsToChannels) of
	{ok, Channel} -> Nick++"!"++Nick++"@"++binary_to_list(Channel);
	_ -> Nick++"!"++Nick++"@"++binary_to_list(Room)
    end.
make_irc_sender(#jid{lresource = Nick} = JID, State) ->
    make_irc_sender(binary_to_list(Nick), JID, State).

user_jid(#state{nick = Nick, host = Host}) ->
    jlib:make_jid(list_to_binary(Nick), Host, <<"irc">>).

filter_cdata(Msg) ->
    [{xmlcdata, filter_message(Msg)}].

filter_message(Msg) ->
    lists:filter(
      fun(C) ->
	      if (C < 32) and
		 %% Add color support, but break XML: (see https://support.process-one.net/browse/EJAB-1097 )
		 %% (C /= 3) and
		 (C /= 9) and
		 (C /= 10) and
		 (C /= 13) ->
		      false;
		 true -> true
	      end
      end, Msg).

translate_action(Msg) ->
    case Msg of
	[1, $A, $C, $T, $I, $O, $N, $  | Action] ->
	    "/me "++Action;
	_ ->
	    Msg
    end.

parse_error({xmlel, "error", _ErrorAttrs, ErrorEls} = ErrorEl) ->
    ErrorTextEl = xml:get_subtag(ErrorEl, "text"),
    ErrorName =
	case ErrorEls -- [ErrorTextEl] of
	    [{xmlel, ErrorReason, _, _}] ->
		ErrorReason;
	    _ ->
		"unknown error"
	end,
    ErrorText =
	case ErrorTextEl of
	    {xmlel, _, _, _} ->
		xml:get_tag_cdata(ErrorTextEl);
	    _ ->
		nothing
    end,
    {ErrorName, ErrorText}.

error_to_string({xmlel, "error", _ErrorAttrs, _ErrorEls} = ErrorEl) ->
    case parse_error(ErrorEl) of
	{ErrorName, ErrorText} when is_list(ErrorText) ->
	    ErrorName ++ ": " ++ ErrorText;
	{ErrorName, _} ->
	    ErrorName
    end;
error_to_string(_) ->
    "unknown error".
