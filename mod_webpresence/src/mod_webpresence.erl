%%%----------------------------------------------------------------------
%%% File    : mod_webpresence.erl
%%% Author  : Igor Goryachev <igor@goryachev.org>, Badlop, runcom <antonio.murdaca@gmail.com>
%%% Purpose : Allow user to show presence in the web
%%% Created : 30 Apr 2006 by Igor Goryachev <igor@goryachev.org>
%%% Id      : $Id: mod_webpresence.erl 1083 2010-06-01 18:32:55Z badlop $
%%%----------------------------------------------------------------------

-module(mod_webpresence).

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2,
         start/2,
         stop/1,
         remove_user/2,
         web_menu_host/3, web_page_host/3,
         process/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("ejabberd_web_admin.hrl").
-include("ejabberd_http.hrl").

-record(webpresence, {us, ridurl = false, jidurl = false, xml = false, avatar = false, js = false, text = false, icon = "---"}).
-record(state, {host, server_host, base_url, access}).
-record(presence, {resource, show, priority, status}).

%% Copied from ejabberd_sm.erl
-record(session, {sid, usr, us, priority, info}).

-define(PROCNAME, ejabberd_mod_webpresence).
-define(PIXMAPS_DIR, <<"pixmaps">>).
-define(AUTO_ACL, webpresence_auto).


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
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
	 temporary, 1000, worker, [?MODULE]},
    Default_dir = case code:priv_dir(ejabberd) of
		      {error, _} -> ?PIXMAPS_DIR;
		      Path -> filename:join([Path, ?PIXMAPS_DIR])
		  end,
    Dir = gen_mod:get_opt(pixmaps_path, Opts, fun(D) -> D end, Default_dir),
    catch ets:new(pixmaps_dirs, [named_table, public]),
    ets:insert(pixmaps_dirs, {directory, Dir}),
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

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
    mnesia:create_table(webpresence,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, webpresence)}]),
    mnesia:add_table_index(webpresence, ridurl),
    update_table(),
    MyHost = gen_mod:get_opt_host(Host, Opts, <<"webpresence.@HOST@">>),
    Access = gen_mod:get_opt(access, Opts, fun(O) -> O end, local),
    Port = gen_mod:get_opt(port, Opts, fun(O) -> O end, 5280),
    Path = gen_mod:get_opt(path, Opts, fun(O) -> O end, <<"presence">>),
    BaseURL = gen_mod:get_opt(baseurl, Opts, fun(O) -> O end,
                              iolist_to_binary(io_lib:format(<<"http://~s:~p/~s/">>, [Host, Port, Path]))),
    ejabberd_router:register_route(MyHost),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(webadmin_menu_host, Host, ?MODULE, web_menu_host, 50),
    ejabberd_hooks:add(webadmin_page_host, Host, ?MODULE, web_page_host, 50),
    {ok, #state{host = MyHost,
		server_host = Host,
		base_url = BaseURL,
		access = Access}}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet},
	    #state{host = Host,
		   server_host = ServerHost,
		   base_url = BaseURL,
		   access = Access} = State) ->
    case catch do_route(Host, ServerHost, Access, From, To, Packet, BaseURL) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ok
    end,
    {noreply, State};


handle_info({tell_baseurl, Pid},
	    #state{base_url = BaseURL} = State) ->
    Pid ! {baseurl_is, BaseURL},
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
terminate(_Reason, #state{host = Host}) ->
    ejabberd_router:unregister_route(Host),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(webadmin_menu_host, Host, ?MODULE, web_menu_host, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host, ?MODULE, web_page_host, 50),
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
do_route(Host, ServerHost, Access, From, To, Packet, BaseURL) ->
    case acl:match_rule(ServerHost, Access, From) of
	allow ->
	    do_route1(Host, From, To, Packet, BaseURL);
	_ ->
	    #xmlel{attrs = Attrs} = Packet,
            Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
	    ErrText = <<"Access denied by service policy">>,
	    Err = jlib:make_error_reply(Packet, ?ERRT_FORBIDDEN(Lang, ErrText)),
	    ejabberd_router:route(To, From, Err)
    end.

do_route1(Host, From, To, Packet, BaseURL) ->
    #xmlel{name = Name, attrs = Attrs} = Packet,
    case Name of
        <<"iq">> -> do_route1_iq(Host, From, To, Packet, BaseURL, jlib:iq_query_info(Packet));
        _ -> case xml:get_attr_s(<<"type">>, Attrs) of
		 <<"error">> -> ok;
		 <<"result">> -> ok;
		 _ -> Err = jlib:make_error_reply(Packet, ?ERR_ITEM_NOT_FOUND),
		      ejabberd_router:route(To, From, Err)
	     end
    end.

do_route1_iq(_, From, To, _, _,
	     #iq{type = get, xmlns = ?NS_DISCO_INFO, lang = Lang} = IQ) ->
    SubEl = #xmlel{
                name = <<"query">>,
                attrs = [{<<"xmlns">>, ?NS_DISCO_INFO}],
                children = iq_disco_info(Lang)
               },
    Res = IQ#iq{type = result, sub_el = [SubEl]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(Res));

do_route1_iq(_, _, _, _, _,
	     #iq{type = get, xmlns = ?NS_DISCO_ITEMS}) ->
    ok;

do_route1_iq(Host, From, To, _, _,
	     #iq{type = get, xmlns = ?NS_REGISTER, lang = Lang} = IQ) ->
    SubEl = #xmlel{
               name = <<"query">>,
               attrs = [{<<"xmlns">>, ?NS_DISCO_INFO}],
               children = iq_get_register_info(Host, From, Lang)
              },
    Res = IQ#iq{type = result, sub_el = [SubEl]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(Res));

do_route1_iq(Host, From, To, Packet, BaseURL,
	     #iq{type = set, xmlns = ?NS_REGISTER, lang = Lang, sub_el = SubEl} = IQ) ->
    case process_iq_register_set(From, SubEl, Host, BaseURL, Lang) of
	{result, IQRes} ->
            SubEl2 = #xmlel{
                        name = <<"query">>,
                        attrs = [{<<"xmlns">>, ?NS_REGISTER}],
                        children = IQRes
                       },
	    Res = IQ#iq{type = result, sub_el = [SubEl2]},
	    ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
	{error, Error} ->
	    Err = jlib:make_error_reply(Packet, Error),
	    ejabberd_router:route(To, From, Err)
    end;

do_route1_iq(_Host, From, To, _, _,
	     #iq{type = get, xmlns = ?NS_VCARD = XMLNS} = IQ) ->
    SubEl = #xmlel{
               name = <<"vCard">>,
               attrs = [{<<"xmlns">>, XMLNS}],
               children = iq_get_vcard()
              },
    Res = IQ#iq{type = result, sub_el = [SubEl]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(Res));

do_route1_iq(_Host, From, To, Packet, _, #iq{}) ->
    Err = jlib:make_error_reply(Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
    ejabberd_router:route(To, From, Err);

do_route1_iq(_, _, _, _, _, _) ->
    ok.

iq_disco_info(Lang) ->
    [#xmlel{
       name = <<"identity">>,
       attrs = [{<<"category">>, <<"component">>},
                {<<"type">>, <<"presence">>},
                {<<"name">>, ?T(<<"Web Presence">>)}],
       children = []
      },
     #xmlel{
        name = <<"feature">>,
        attrs = [{<<"var">>, ?NS_REGISTER}],
        children = []
       },
     #xmlel{
        name = <<"feature">>,
        attrs = [{<<"var">>, ?NS_VCARD}],
        children = []
       }].

-define(XFIELDS(Type, Label, Var, Vals),
        #xmlel{
           name = <<"field">>,
           attrs = [
                    {<<"type">>, Type},
                    {<<"label">>, ?T(Label)},
                    {<<"var">>, Var}
                   ],
           children = Vals
          }).

-define(XFIELD(Type, Label, Var, Val),
	?XFIELDS(Type, Label, Var,
		 [
                  #xmlel{
                     name = <<"value">>,
                     attrs = [],
                     children = [{xmlcdata, Val}]
                    }])
       ).

-define(XFIELDFIXED(Val),
        #xmlel{
           name = <<"field">>,
           attrs = [{<<"type">>, <<"fixed">>}],
           children  = [
                        #xmlel{
                           name = <<"value">>,
                           attrs = [],
                           children = [{xmlcdata, Val}]
                          }
                       ]
          }
       ).

-define(ATOM2BINARY(Val), iolist_to_binary(atom_to_list(Val))).
-define(BC(L), iolist_to_binary(L)).

ridurl_out(false) -> <<"false">>;
ridurl_out(Id) when is_binary(Id) -> <<"true">>.

to_bool(<<"false">>) -> false;
to_bool(<<"true">>) -> true;
to_bool(<<"0">>) -> false;
to_bool(<<"1">>) -> true.

get_pr(LUS) ->
    case catch mnesia:dirty_read(webpresence, LUS) of
	[#webpresence{jidurl = J, ridurl = H, xml = X, avatar = A, js = S, text = T, icon = I}] ->
	    {J, H, X, A, S, T, I, true};
	_ ->
	    {true, false, false, false, false, false, <<"---">>, false}
    end.

get_pr_rid(LUS) ->
    {_, H, _, _, _, _, _, _} = get_pr(LUS),
    H.

iq_get_register_info(_Host, From, Lang) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    LUS = {LUser, LServer},
    {JidUrl, RidUrl, XML, Avatar, JS, Text, Icon, Registered} = get_pr(LUS),
    RegisteredXML = case Registered of
			true -> [#xmlel{name = <<"registered">>, attrs = [], children = []}];
			false -> []
		    end,
    RegisteredXML ++
        [
         #xmlel{
            name = <<"instructions">>,
            attrs = [],
            children = [{xmlcdata, ?T(<<"You need an x:data capable client to register presence">>)}]
           },
         #xmlel{
            name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_XDATA}],
            children = [
                        #xmlel{
                           name = <<"title">>,
                           attrs = [],
                           children = [{xmlcdata, ?T(<<"Web Presence">>)}]
                          },
                        #xmlel{
                           name = <<"instructions">>,
                           attrs = [],
                           children = [{xmlcdata, ?T(<<"What features do you want to enable?">>)}]
                          },
                        ?XFIELDFIXED(?BC([?T(<<"URL Type">>), <<". ">>, ?T(<<"Select one at least">>)])),
                        ?XFIELD(<<"boolean">>, <<"Jabber ID">>, <<"jidurl">>, ?ATOM2BINARY(JidUrl)),
                        ?XFIELD(<<"boolean">>, <<"Random ID">>, <<"ridurl">>, ridurl_out(RidUrl)),
                        ?XFIELDFIXED(?BC([?T(<<"Output Type">>), <<". ">>, ?T(<<"Select one at least">>)])),
                        ?XFIELDS(<<"list-single">>, ?T(<<"Icon Theme">>), <<"icon">>,
                                 [
                                  #xmlel{
                                     name = <<"value">>,
                                     attrs = [],
                                     children = [{xmlcdata, Icon}]
                                    },
                                  #xmlel{
                                     name = <<"option">>,
                                     attrs = [{<<"label">>, <<"---">>}],
                                     children = [
                                                 #xmlel{
                                                    name = <<"value">>,
                                                    attrs = [],
                                                    children = [{xmlcdata, <<"---">>}]
                                                   }
                                                ]
                                    }
                                ] ++ available_themes(xdata)
                                ),
                        ?XFIELD(<<"boolean">>, <<"XML">>, <<"xml">>, ?ATOM2BINARY(XML)),
                        ?XFIELD(<<"boolean">>, <<"JavaScript">>, <<"js">>, ?ATOM2BINARY(JS)),
                        ?XFIELD(<<"boolean">>, <<"Text">>, <<"text">>, ?ATOM2BINARY(Text)),
                        ?XFIELD(<<"boolean">>, <<"Avatar">>, <<"avatar">>, ?ATOM2BINARY(Avatar))
                       ]
           }
        ].

%%%% TODO: Check if remote users are allowed to reach here: they should not be allowed
iq_set_register_info(From, {Host, JidUrl, RidUrl, XML, Avatar, JS, Text, Icon, _, Lang} = Opts) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    LUS = {LUser, LServer},
    Check_URLTypes = (JidUrl == true) or (RidUrl =/= false),
    Check_OutputTypes = (XML == true) or (Avatar == true) or (JS == true) or (Text == true) or (Icon =/= <<"---">>),
    case Check_URLTypes and Check_OutputTypes of
	true -> iq_set_register_info2(From, LUS, Opts);
	false -> unregister_webpresence(From, Host, Lang)
    end.

iq_set_register_info2(From, LUS, {Host, JidUrl, RidUrl, XML, Avatar, JS, Text, Icon, BaseURL, Lang}) ->
    RidUrl2 = get_rid_final_value(RidUrl, LUS),
    WP = #webpresence{us = LUS,
		      jidurl = JidUrl,
		      ridurl = RidUrl2,
		      xml = XML,
		      avatar = Avatar,
		      js = JS,
		      text = Text,
		      icon = Icon},
    F = fun() -> mnesia:write(WP) end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    send_message_registered(WP, From, Host, BaseURL, Lang),
	    {result, []};
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

get_rid_final_value(false, _) -> false;
get_rid_final_value(true, {U, S} = LUS) ->
    case get_pr_rid(LUS) of
	false ->
	    iolist_to_binary(integer_to_list(erlang:phash2(U) * erlang:phash2(S)
			    * calendar:datetime_to_gregorian_seconds(
				calendar:local_time()))
		++ randoms:get_string());
	H when is_binary(H) ->
	    H
    end.

send_message_registered(WP, To, Host, BaseURL, Lang) ->
    {User, Server} = WP#webpresence.us,
    JID = jlib:make_jid(User, Server, <<"">>),
    JIDS = jlib:jid_to_string(JID),
    Oavatar = case WP#webpresence.avatar of
		  false -> <<"">>;
		  true -> <<"  avatar\n"
			    "  avatar/my.png\n">>
	      end,
    Ojs = case WP#webpresence.js of
	      false -> <<"">>;
	      true -> <<"  js\n">>
	  end,
    Otext = case WP#webpresence.text of
		false -> <<"">>;
		true -> ?BC([
                        <<"  text\n"
			  "  text/res/<">>, ?T(<<"Resource">>), <<">\n">>
                                         ])
	    end,
    Oimage = case WP#webpresence.icon of
		 <<"---">> -> "";
		 I when is_binary(I) ->
                     ?BC([
		     <<"  image\n"
		       "  image/example.php\n"
		       "  image/mypresence.png\n"
		       "  image/res/<">>, ?T(<<"Resource">>), <<">\n"
		       "  image/theme/<">>, ?T(<<"Icon Theme">>), <<">\n"
		       "  image/theme/<">>, ?T(<<"Icon Theme">>), <<">/res/<">>, ?T(<<"Resource">>), <<">\n">>
                                      ])
	     end,
    Oxml = case WP#webpresence.xml of
	       false -> <<"">>;
	       true -> <<"  xml\n">>
	   end,
    Allowed_type = case {Oimage, Oxml, Oavatar, Otext, Ojs} of
		       {<<"">>, <<"">>, <<"">>, <<"">>, _} -> <<"js">>;
		       {<<"">>, <<"">>, <<"">>, _, _} -> <<"text">>;
		       {<<"">>, <<"">>, _, _, _} -> <<"avatar">>;
		       {<<"">>, _, _, _, _} -> <<"xml">>;
		       {_, _, _, _, _} -> <<"image">>
		   end,
    {USERID_jid, Example_jid} = case WP#webpresence.jidurl of
				    false -> {<<"">>, <<"">>};
				    true ->
					JIDT = ?BC([<<"jid/">>, User, <<"/">>, Server]),
					{?BC([<<"  ">>, JIDT, <<"\n">>]),
					 ?BC([<<"  ">>, BaseURL, JIDT, <<"/">>, Allowed_type, <<"/\n">>])}
				end,
    {USERID_rid, Example_rid, Text_rid} = case WP#webpresence.ridurl of
					      false -> {<<"">>, <<"">>, <<"">>};
					      RID when is_binary(RID) ->
						  RIDT = ?BC([<<"rid/">>, RID]),
						  {?BC([<<"  ">>, RIDT, <<"\n">>]),
						   ?BC([<<"  ">>, BaseURL, RIDT, <<"/">>, Allowed_type, <<"/\n">>]),
						   ?BC([?T(<<"If you forget your RandomID, register again to receive this message.">>), <<"\n">>,
						   ?T(<<"To get a new RandomID, disable the option and register again.">>), <<"\n">>])
						  }
					  end,
    Subject = ?BC([?T(<<"Web Presence">>), <<": ">>, ?T(<<"registered">>)]),
    Body = ?BC([?T(<<"You have registered:">>), <<" ">>, JIDS, <<"\n\n">>,
	?T(<<"Use URLs like:">>), <<"\n">>,
	<<"  ">>, BaseURL, <<"USERID/OUTPUT/\n">>,
	<<"\n">>,
	<<"USERID:\n">>, USERID_jid, USERID_rid, <<"\n">>,
	<<"OUTPUT:\n">>, Oimage, Oxml, Ojs, Otext, Oavatar, <<"\n">>,
	?T(<<"Example:">>), <<"\n">>, Example_jid, Example_rid, <<"\n">>,
	Text_rid]),
    send_headline(Host, To, Subject, Body).

send_message_unregistered(To, Host, Lang) ->
    Subject = ?BC([?T(<<"Web Presence">>), <<": ">>, ?T(<<"unregistered">>)]),
    Body = ?BC([?T(<<"You have unregistered.">>), <<"\n\n">>]),
    send_headline(Host, To, Subject, Body).

send_headline(Host, To, Subject, Body) ->
    ejabberd_router:route(
      jlib:make_jid(<<"">>, Host, <<"">>),
      To,
      #xmlel{
         name = <<"message">>,
         attrs = [{<<"type">>, <<"headline">>}],
         children = [
                     #xmlel{
                        name = <<"subject">>,
                        attrs = [],
                        children = [{xmlcdata, Subject}]
                       },
                     #xmlel{
                        name = <<"body">>,
                        attrs = [],
                        children = [{xmlcdata, Body}]
                       }
                    ]
        }).

get_attr(Attr, XData, Default) ->
    case lists:keysearch(Attr, 1, XData) of
	{value, {_, [Value]}} -> Value;
	false -> Default
    end.

process_iq_register_set(From, SubEl, Host, BaseURL, Lang) ->
    #xmlel{name = _, attrs = _, children = Els} = SubEl,
    case xml:get_subtag(SubEl, <<"remove">>) of
	false -> case catch process_iq_register_set2(From, Els, Host, BaseURL, Lang) of
		     {'EXIT', _} -> {error, ?ERR_BAD_REQUEST};
		     R -> R
		 end;
	_ -> unregister_webpresence(From, Host, Lang)
    end.

process_iq_register_set2(From, Els, Host, BaseURL, Lang) ->
    [
     #xmlel{
        name = <<"x">>,
        attrs = _Attrs1,
        children = _Els1
       } = XEl
    ] = xml:remove_cdata(Els),
    case {xml:get_tag_attr_s(<<"xmlns">>, XEl), xml:get_tag_attr_s(<<"type">>, XEl)} of
	{?NS_XDATA, <<"cancel">>} ->
	    {result, []};
	{?NS_XDATA, <<"submit">>} ->
	    XData = jlib:parse_xdata_submit(XEl),
	    false = (invalid == XData),
	    JidUrl = get_attr(<<"jidurl">>, XData, <<"false">>),
	    RidUrl = get_attr(<<"ridurl">>, XData, <<"false">>),
	    XML = get_attr(<<"xml">>, XData, <<"false">>),
	    Avatar = get_attr(<<"avatar">>, XData, <<"false">>),
	    JS = get_attr(<<"js">>, XData, <<"false">>),
	    Text = get_attr(<<"text">>, XData, <<"false">>),
	    Icon = get_attr(<<"icon">>, XData, <<"---">>),
	    iq_set_register_info(From, {Host, to_bool(JidUrl), to_bool(RidUrl), to_bool(XML), to_bool(Avatar), to_bool(JS), to_bool(Text), Icon, BaseURL, Lang})
    end.

unregister_webpresence(From, Host, Lang) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    remove_user(LUser, LServer),
    send_message_unregistered(From, Host, Lang),
    {result, []}.

remove_user(User, Server) ->
    mnesia:dirty_delete(webpresence, {User, Server}).

iq_get_vcard() ->
    [
     #xmlel{
        name = <<"FN">>,
        attrs = [],
        children = [{xmlcdata, <<"ejabberd/mod_webpresence">>}]
       },
     #xmlel{
        name = <<"URL">>,
        attrs = [],
        children = [{xmlcdata, <<"http://www.ejabberd.im/mod_webpresence">>}]
       },
     #xmlel{
        name = <<"DESC">>,
        attrs = [],
        children = [{xmlcdata, <<"ejabberd web presence module\nCopyright (c) 2006-2007 Igor Goryachev, 2007 Badlop, 2014 runcom <antonio.murdaca@gmail.com>">>}]
       }
    ].

get_wp(LUser, LServer) ->
    LUS = {LUser, LServer},
    case catch mnesia:dirty_read(webpresence, LUS) of
        {'EXIT', _Reason} ->
	    try_auto_webpresence(LUser, LServer);
        [] ->
	    try_auto_webpresence(LUser, LServer);
	[WP] when is_record(WP, webpresence) ->
	    WP
    end.

try_auto_webpresence(LUser, LServer) ->
    From = jlib:make_jid(LUser, LServer, <<"">>),
    case acl:match_rule(LServer, ?AUTO_ACL, From) of
	deny ->
	    #webpresence{};
	allow ->
	    #webpresence{us = {LUser, LServer},
			 ridurl = false,
			 jidurl = true,
			 xml = true,
			 avatar = true,
			 js = true,
			 text = true,
			 icon = <<"jsf-jabber-text">>}
   end.

get_status_weight(Show) ->
    case Show of
        <<"chat">>        -> 0;
        <<"available">>   -> 1;
        <<"away">>        -> 2;
        <<"xa">>          -> 3;
        <<"dnd">>         -> 4;
        _                 -> 9
    end.

session_to_presence(#session{sid = {_, Pid}, priority = Priority}) ->
    {_User, Resource, Show, Status} = ejabberd_c2s:get_presence(Pid),
    #presence{resource = Resource,
              show = Show,
              priority = Priority,
              status = Status}.

get_presences({bare, LUser, LServer}) ->
    [session_to_presence(Session) ||
        Session <- mnesia:dirty_index_read(session, {LUser, LServer}, #session.us)];

get_presences({sorted, LUser, LServer}) ->
    lists:sort(
      fun(A, B) ->
              if
                  A#presence.priority == B#presence.priority ->
                      WA = get_status_weight(A#presence.show),
                      WB = get_status_weight(B#presence.show),
                      WA < WB;
                  true ->
                      A#presence.priority > B#presence.priority
              end
      end,
      get_presences({bare, LUser, LServer}));

get_presences({xml, LUser, LServer, Show_us}) ->
    #xmlel{
       name = <<"presence">>,
       attrs = case Show_us of
                   true -> [{<<"user">>, LUser}, {<<"server">>, LServer}];
                   false -> []
               end,
       children = lists:map(
                    fun(Presence) ->
                            #xmlel{
                               name = <<"resource">>,
                               attrs = [
                                        {<<"name">>, Presence#presence.resource},
                                        {<<"show">>, Presence#presence.show},
                                        {<<"priority">>, iolist_to_binary(integer_to_list(Presence#presence.priority))}
                                       ],
                               children = [{xmlcdata, Presence#presence.status}]
                              }
                    end,
                    get_presences({sorted, LUser, LServer})
                   )
      };

get_presences({status, LUser, LServer, LResource}) ->
    case get_presences({sorted, LUser, LServer}) of
	[] -> <<"unavailable">>;
	Rs ->
	    {value, R} = lists:keysearch(LResource, 2, Rs),
	    R#presence.show %% TODO why was this "status"?!
    end;

get_presences({status, LUser, LServer}) ->
    case get_presences({sorted, LUser, LServer}) of
        [Highest | _Rest] ->
            Highest#presence.show; %% TODO why was this "status"?!
        _ ->
            <<"unavailable">>
    end;

get_presences({show, LUser, LServer, LResource}) ->
    case get_presences({sorted, LUser, LServer}) of
	[] -> <<"unavailable">>;
	Rs ->
	    {value, R} = lists:keysearch(LResource, 2, Rs),
	    R#presence.show
    end;

get_presences({show, LUser, LServer}) ->
    case get_presences({sorted, LUser, LServer}) of
        [Highest | _Rest] ->
            Highest#presence.show;
        _ ->
            <<"unavailable">>
    end.

make_js(WP, Prs, Show_us, Lang, Q) ->
    {User, Server} = WP#webpresence.us,
    BaseURL = get_baseurl(Server),
    US_string = case Show_us of
		    true ->
			?BC([<<"var jabber_user='">>, User, <<"';\n">>, <<"var jabber_server='">>, Server, <<"';\n">>]); false -> <<"">>
		end,
    FunImage = fun(I, S) ->
		       case I of
			   <<"---">> -> <<"">>;
			   Icon -> ?BC([<<" image:'">>, BaseURL, <<"image/">>, Icon, <<"/">>, S, <<"'\n">>])
		       end
	       end,
    R_string_list = case Prs of
			[] ->
			    Show = <<"unavailable">>,
			    [?BC([<<"{show:'">>, Show, <<"',\n">>,
			     <<" long_show:'">>, long_show(Show, Lang), <<"',\n">>,
			     <<" status:'',\n">>, % TODO
			     FunImage(WP#webpresence.icon, Show),
			     <<"}">>])];
			_ -> lists:map(
			       fun(Pr) ->
				       Show =  Pr#presence.show,
				       ?BC([<<"{name:'">>, Pr#presence.resource, <<"',\n">>,
					   <<" priority:">>, intund2string(Pr#presence.priority), <<",\n">>,
					   <<" show:'">>, Show, <<"',\n">>,
					   <<" long_show:'">>, long_show(Show, Lang), <<"',\n">>,
					   <<" status:'">>, escape(Pr#presence.status), <<"',\n">>,
					   FunImage(WP#webpresence.icon, Show),
					   <<"}">>])
			       end,
			       Prs)
		    end,
    R_string = lists:foldl(
		 fun(RS, Res) ->
			 case Res of
			     <<"">> -> RS;
			     _ -> ?BC([Res, <<",\n">>, RS])
			 end
		 end,
		 <<"">>,
		 R_string_list),
    CB_string = case lists:keysearch(<<"cb">>, 1, Q) of
                    {value, {_, CB}} -> ?BC([<<" ">>, CB, <<"();">>]);
                    _                -> <<"">>
                end,
    ?BC([US_string, <<"var jabber_resources=[\n">>, R_string, <<"];">>, CB_string]).

long_show(<<"available">>, Lang) -> ?T(<<"available">>);
long_show(<<"chat">>, Lang) -> ?T(<<"free for chat">>);
long_show(<<"away">>, Lang) -> ?T(<<"away">>);
long_show(<<"xa">>, Lang) -> ?T(<<"extended away">>);
long_show(<<"dnd">>, Lang) -> ?T(<<"do not disturb">>);
long_show(_, Lang) -> ?T(<<"unavailable">>).

intund2string(undefined) -> <<"undefined">>;
intund2string(Int) when is_integer(Int) -> list_to_binary(integer_to_list(Int)).

escape(S1) ->
    S2 = re:replace(S1, "\'", "\\'", [global, {return, list}]),
    re:replace(S2, "\n", "\\n", [global, {return, list}]).

get_baseurl(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! {tell_baseurl, self()},
    receive
	{baseurl_is, BaseURL} -> BaseURL
    end.

-define(XML_HEADER, <<"<?xml version='1.0' encoding='utf-8'?>">>).

get_pixmaps_directory() ->
    [{directory, Path} | _] = ets:lookup(pixmaps_dirs, directory),
    Path.

available_themes(list) ->
    case file:list_dir(get_pixmaps_directory()) of
        {ok, List} ->
            L2 = lists:sort(List),
	    %% Remove from the list of themes the directories that start with a dot
	    [T || T <- L2, hd(T) =/= 46];
        {error, _} ->
            []
    end;

available_themes(xdata) ->
    lists:map(
      fun(Theme) ->
              #xmlel{
                 name = <<"option">>,
                 attrs = [{<<"label">>, iolist_to_binary(Theme)}],
                 children = [
                             #xmlel{
                                name = <<"value">>,
                                attrs = [],
                                children = [{xmlcdata, iolist_to_binary(Theme)}]
                               }
                            ]
                }
      end, available_themes(list)).

show_presence({image_no_check, Theme, Pr}) ->
    Dir = get_pixmaps_directory(),
    Image = ?BC([Pr, <<".{gif,png,jpg}">>]),
    [First | _Rest] = filelib:wildcard(binary_to_list(filename:join([Dir, Theme, Image]))),
    Mime = string:substr(First, string:len(First) - 2, 3),
    {ok, Content} = file:read_file(First),
    {200, [{<<"Content-Type">>, ?BC([<<"image/">>, ?BC(Mime)])}], binary_to_list(Content)};

show_presence({image, WP, LUser, LServer}) ->
    Icon = WP#webpresence.icon,
    false = (<<"---">> == Icon),
    Pr = get_presences({show, LUser, LServer}),
    show_presence({image_no_check, Icon, Pr});

show_presence({image, WP, LUser, LServer, Theme}) ->
    false = (<<"---">> == WP#webpresence.icon),
    Pr = get_presences({show, LUser, LServer}),
    show_presence({image_no_check, Theme, Pr});

show_presence({image_res, WP, LUser, LServer, LResource}) ->
    Icon = WP#webpresence.icon,
    false = (<<"---">> == Icon),
    Pr = get_presences({show, LUser, LServer, LResource}),
    show_presence({image_no_check, Icon, Pr});

show_presence({image_res, WP, LUser, LServer, Theme, LResource}) ->
    false = (<<"---">> == WP#webpresence.icon),
    Pr = get_presences({show, LUser, LServer, LResource}),
    show_presence({image_no_check, Theme, Pr});

show_presence({xml, WP, LUser, LServer, Show_us}) ->
    true = WP#webpresence.xml,
    Presence_xml = xml:element_to_binary(get_presences({xml, LUser, LServer, Show_us})),
    {200, [{"Content-Type", "text/xml; charset=utf-8"}], ?BC([?XML_HEADER, Presence_xml])};

show_presence({js, WP, LUser, LServer, Show_us, Lang, Q}) ->
    true = WP#webpresence.js,
    Prs = get_presences({sorted, LUser, LServer}),
    Js = make_js(WP, Prs, Show_us, Lang, Q),
    {200, [{"Content-Type", "text/html; charset=utf-8"}], Js};

show_presence({text, WP, LUser, LServer}) ->
    true = WP#webpresence.text,
    Presence_text = get_presences({status, LUser, LServer}),
    {200, [{"Content-Type", "text/html; charset=utf-8"}], Presence_text};

show_presence({text, WP, LUser, LServer, LResource}) ->
    true = WP#webpresence.text,
    Presence_text = get_presences({status, LUser, LServer, LResource}),
    {200, [{"Content-Type", "text/html; charset=utf-8"}], Presence_text};

show_presence({avatar, WP, LUser, LServer}) ->
    true = WP#webpresence.avatar,
    [{_, Module, Function, _Opts}] = ets:lookup(sm_iqtable, {?NS_VCARD, LServer}),
    JID = jlib:make_jid(LUser, LServer, <<"">>),
    IQ = #iq{type = get, xmlns = ?NS_VCARD},
    IQr = Module:Function(JID, JID, IQ),
    [VCard] = IQr#iq.sub_el,
    Mime = xml:get_path_s(VCard, [{elem, <<"PHOTO">>}, {elem, <<"TYPE">>}, cdata]),
    BinVal = xml:get_path_s(VCard, [{elem, <<"PHOTO">>}, {elem, <<"BINVAL">>}, cdata]),
    Photo = jlib:decode_base64(BinVal),
    {200, [{"Content-Type", Mime}], Photo};

show_presence({image_example, Theme, Show}) ->
    Dir = get_pixmaps_directory(),
    Image = ?BC([Show, <<".{gif,png,jpg}">>]),
    [First | _Rest] = filelib:wildcard(binary_to_list(filename:join([Dir, Theme, Image]))),
    Mime = string:substr(First, string:len(First) - 2, 3),
    {ok, Content} = file:read_file(First),
    {200, [{<<"Content-Type">>, ?BC([<<"image/">>, ?BC(Mime)])}], binary_to_list(Content)}.

%% ---------------------
%% Web Publish
%% ---------------------

make_xhtml(Els) -> make_xhtml([], Els).
make_xhtml(Title, Els) ->
    #xmlel{
       name = <<"html">>,
       attrs = [
                {<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>},
                {<<"xml:lang">>, <<"en">>},
                {<<"lang">>, <<"en">>}
               ],
       children = [
                   #xmlel{
                      name = <<"head">>,
                      attrs = [],
                      children = [
                                  #xmlel{
                                     name = <<"meta">>,
                                     attrs = [
                                              {<<"http-equiv">>, <<"Content-Type">>},
                                              {<<"content">>, <<"text/html; charset=utf-8">>}
                                             ],
                                     children = []
                                    }
                                 ] ++ Title
                     },
                   #xmlel{
                      name = <<"body">>,
                      attrs = [],
                      children = Els
                     }
                  ]
      }.

themes_to_xhtml(Themes) ->
    ShowL = ["available", "chat", "dnd", "away", "xa", "unavailable"],
    THeadL = [""] ++ ShowL,
    [?XAE(<<"table">>, [],
          [?XE( <<"tr">>, [?XC(<<"th">>, ?BC(T)) || T <- THeadL])] ++
	  [?XE(<<"tr">>, [?XC(<<"td">>, ?BC(Theme)) |
		      [?XE(<<"td">>, [?XA(<<"img">>, [{<<"src">>, ?BC([<<"image/">>, ?BC(Theme), <<"/">>, ?BC(T)]) }])]) || T <- ShowL]
		     ]
	      ) || Theme <- Themes]
	 )
    ].

parse_lang(Lang) -> iolist_to_binary(hd(string:tokens(binary_to_list(Lang),"-"))).

process(LocalPath, Request) ->
    case catch process2(LocalPath, Request) of
	{'EXIT', Reason} ->
            ?DEBUG("~p", [Request]),
	    ?DEBUG("The call to path ~p in the~nrequest: ~p~ncrashed with error: ~p", [LocalPath, Request, Reason]),
	    {404, [], make_xhtml([?XC(<<"h1">>, <<"Not found">>)])};
	Res ->
	    Res
    end.

process2([], #request{lang = Lang1}) ->
    Lang = parse_lang(Lang1),
    Title = [?XC(<<"title">>, ?T(<<"Web Presence">>))],
    Desc = [?XC(<<"p">>, ?BC([ ?T(<<"To publish your presence using this system you need a Jabber account in this Jabber server.">>), <<" ">>,
		?T(<<"Login with a Jabber client, open the Service Discovery and register in Web Presence.">>),
		?T(<<"You will receive a message with further instructions.">>)]))],
    Link_themes = [?AC(<<"themes">>, ?T(<<"Icon Theme">>))],
    Body = [?XC(<<"h1">>, ?T(<<"Web Presence">>))] ++ Desc ++ Link_themes,
    make_xhtml(Title, Body);

process2([<<"themes">>], #request{lang = Lang1}) ->
    Lang = parse_lang(Lang1),
    Title = [?XC(<<"title">>, ?BC([?T(<<"Web Presence">>), <<" - ">>, ?T("Icon Theme")]))],
    Themes = available_themes(list),
    Icon_themes = themes_to_xhtml(Themes),
    Body = [?XC(<<"h1">>, ?T(<<"Icon Theme">>))] ++ Icon_themes,
    make_xhtml(Title, Body);

process2([<<"image">>, Theme, Show], #request{} = _Request) ->
    Args = {image_example, Theme, Show},
    show_presence(Args);

process2([<<"jid">>, User, Server | Tail], Request) ->
    serve_web_presence(jid, User, Server, Tail, Request);

process2([<<"rid">>, Rid | Tail], Request) ->
    [Pr] = mnesia:dirty_index_read(webpresence, Rid, #webpresence.ridurl),
    {User, Server} = Pr#webpresence.us,
    serve_web_presence(rid, User, Server, Tail, Request);

%% Compatibility with old mod_presence
process2([User, Server | Tail], Request) ->
    serve_web_presence(jid, User, Server, Tail, Request).

serve_web_presence(TypeURL, User, Server, Tail, #request{lang = Lang1, q = Q}) ->
    LServer = jlib:nameprep(Server),
    true = lists:member(Server, ?MYHOSTS),
    LUser = jlib:nodeprep(User),
    WP = get_wp(LUser, LServer),
    case TypeURL of
	jid -> true = WP#webpresence.jidurl;
	rid -> true = is_binary(WP#webpresence.ridurl)
    end,
    Show_us = (TypeURL == jid),
    Lang = parse_lang(Lang1),
    Args = case Tail of
               [<<"image">>, <<"theme">>, Theme, <<"res">>, Resource | _] ->
                   {image_res, WP, LUser, LServer, Theme, Resource};
               [<<"image">>, <<"theme">>, Theme | _] ->
                   {image, WP, LUser, LServer, Theme};
               [<<"image">>, <<"res">>, Resource | _] ->
                   {image_res, WP, LUser, LServer, Resource};
               [<<"image">> | _] ->
                   {image, WP, LUser, LServer};
               [<<"xml">>] ->
                   {xml, WP, LUser, LServer, Show_us};
               [<<"js">>] ->
                   {js, WP, LUser, LServer, Show_us, Lang, Q};
               [<<"text">>] ->
                   {text, WP, LUser, LServer};
               [<<"text">>, <<"res">>, Resource] ->
                   {text, WP, LUser, LServer, Resource};
               [<<"avatar">> | _] ->
                   {avatar, WP, LUser, LServer}
	   end,
    show_presence(Args).


%%%% ---------------------
%%%% Web Admin
%%%% ---------------------

web_menu_host(Acc, _Host, Lang) ->
    [{<<"webpresence">>, ?T(<<"Web Presence">>)} | Acc].

web_page_host(_, _Host,
	      #request{path = [<<"webpresence">>],
		       lang = Lang} = _Request) ->
    Res = [?XCT(<<"h1">>, <<"Web Presence">>),
	   ?XE(<<"ul">>, [
		      ?LI([?ACT(<<"stats">>, <<"Statistics">>)]),
		      ?LI([?ACT(<<"users">>, <<"Registered Users">>)])])],
    {stop, Res};

web_page_host(_, Host,
	      #request{path = [<<"webpresence">>, <<"users">>],
		       lang = Lang} = _Request) ->
    Users = get_users(Host),
    Table = make_users_table(Users, Lang),
    Res = [?XCT(<<"h1">>, <<"Web Presence">>),
	   ?XCT(<<"h2">>, <<"Registered Users">>)] ++ Table,
    {stop, Res};

web_page_host(_, Host,
	      #request{path = [<<"webpresence">>, <<"stats">>],
		       lang = Lang} = _Request) ->
    Users = get_users(Host),
    Res = [?XCT(<<"h1">>, <<"Web Presence">>),
	   css_table(),
	   ?XCT(<<"h2">>, <<"Statistics">>)]
	++ make_stats_options(Users, Lang)
	++ make_stats_iconthemes(Users, Lang),
    {stop, Res};

web_page_host(Acc, _, _) -> Acc.

get_users(Host) ->
    Select = [{{webpresence, {'$1', Host}, '$2', '$3', '$4', '$5', '$6', '$7', '$8'}, [], ['$$']}],
    mnesia:dirty_select(webpresence, Select).

make_users_table(Records, Lang) ->
    TList = lists:map(
	      fun([User, RidUrl, JIDUrl, XML, Avatar, JS, Text, Icon]) ->
		      ?XE(<<"tr">>,
			  [?XE(<<"td">>, [?AC(?BC([<<"../user/">>, User, <<"/">>]), User)]),
			   ?XC(<<"td">>, iolist_to_binary(atom_to_list(JIDUrl))),
			   ?XC(<<"td">>, ridurl_out(RidUrl)),
			   ?XC(<<"td">>, Icon),
			   ?XC(<<"td">>, iolist_to_binary(atom_to_list(XML))),
			   ?XC(<<"td">>, iolist_to_binary(atom_to_list(JS))),
			   ?XC(<<"td">>, iolist_to_binary(atom_to_list(Text))),
			   ?XC(<<"td">>, iolist_to_binary(atom_to_list(Avatar)))])
	      end, Records),
    [?XE(<<"table">>,
	 [?XE(<<"thead">>,
	      [?XE(<<"tr">>,
		   [?XCT(<<"td">>, <<"User">>),
		    ?XCT(<<"td">>, <<"Jabber ID">>),
		    ?XCT(<<"td">>, <<"Random ID">>),
		    ?XCT(<<"td">>, <<"Icon Theme">>),
		    ?XC(<<"td">>, <<"XML">>),
		    ?XC(<<"td">>, <<"JS">>),
		    ?XCT(<<"td">>, <<"Text">>),
		    ?XCT(<<"td">>, <<"Avatar">>)
		   ])]),
	  ?XE(<<"tbody">>, TList)])].

make_stats_options(Records, Lang) ->
    [RegUsers, JJ, RR, XX, AA, SS, TT, II] = lists:foldl(
					       fun([_User, RidUrl, JidUrl, XML, Avatar, JS, Text, Icon], [N, J, R, X, A, S, T, I]) ->
						       J2 = J + case JidUrl of false -> 0; true -> 1 end,
						       R2 = R + case RidUrl of false -> 0; _ -> 1 end,
						       X2 = X + case XML of false -> 0; true -> 1 end,
						       A2 = A + case Avatar of false -> 0; true -> 1 end,
						       S2 = S + case JS of false -> 0; true -> 1 end,
						       T2 = T + case Text of false -> 0; true -> 1 end,
						       I2 = I + case Icon of <<"---">> -> 0; _ -> 1 end,
						       [N+1, J2, R2, X2, A2, S2, T2, I2]
					       end,
					       [0, 0, 0, 0, 0, 0, 0, 0],
					       Records),
    URLTList = [{<<"Jabber ID">>, JJ}, {<<"Random ID">>, RR}],
    OutputTList = [{<<"Icon Theme">>, II}, {<<"XML">>, XX}, {<<"JavaScript">>, SS}, {<<"Text">>, TT}, {<<"Avatar">>, AA}],
    [
     ?C(?BC([<<"Registered Users">>, <<": ">>, iolist_to_binary(integer_to_list(RegUsers))])),
     ?XCT(<<"h3">>, <<"URL Type">>),
     ?XAE(<<"table">>, [{<<"class">>, <<"stats">>}],
	  [?XE(<<"tbody">>, do_stat_table_with(URLTList, RegUsers))]
	 ),
     ?XCT(<<"h3">>, <<"Output Type">>),
     ?XAE(<<"table">>, [{<<"class">>, <<"stats">>}],
	  [?XE(<<"tbody">>, do_stat_table_with(OutputTList, RegUsers))]
	 )].

make_stats_iconthemes(Records, Lang) ->
    Themes1 = [{T, 0} || T <- available_themes(list)],
    Dict = lists:foldl(
	     fun([_, _, _, _, _, _, _, Icon], D) ->
		     dict:update_counter(Icon, 1, D)
	     end,
	     dict:from_list(Themes1),
	     Records),
    Themes = lists:keysort(1, dict:to_list(Dict)),
    [?XCT(<<"h3">>, <<"Icon Theme">>),
     ?XAE(<<"table">>, [{<<"class">>, <<"stats">>}],
	  [?XE(<<"tbody">>, do_stat_table_with(Themes))]
	 )].

%% Do table with bars
do_stat_table_with(Values) ->
    Ns = [Ni || {_, Ni} <- Values],
    Total = lists:sum(Ns),
    do_stat_table_with(Values, Total).

do_stat_table_with(Values, Total) ->
    lists:map(
      fun({L, N}) ->
	      Perc = case Total of
			 0 -> <<"0">>;
			 _ -> iolist_to_binary(integer_to_list(trunc(100 * N / Total)))
		     end,
	      do_table_element(?C(L), io_lib:format("~p", [N]), Perc)
      end,
      Values).
do_table_element(L, [N], Perc) ->
    ?XE(<<"tr">>,
	[?XE(<<"td">>, [L]),
	 ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}], [N]),
	 ?XE(<<"td">>,
	     [?XAE(<<"div">>,
		   [{<<"class">>, <<"graph">>}],
		   [?XAC(<<"div">>,
			 [{<<"class">>, <<"bar">>}, {<<"style">>, ?BC([<<"width: ">>, Perc, <<"%;">>])}],
			 []
			)]
		  )]
	    ),
	 ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}], [?BC([Perc, <<"%">>])])
	]).

css_table()->
    ?XAE(<<"style">>, [{<<"type">>, <<"text/css">>}],
	 [?C(<<".stats {
             padding-left: 20px;
	     padding-top: 10px;
	    }
	  .graph {
	     position: relative;
	     width: 200px;
	     border: 1px solid #D47911;
	     padding: 1px;
	    }
	  .graph .bar {
		    display: block;
		    position: relative;
		    background: #FFE3C9;
		    text-align: center;
		    color: #333;
		    height: 1.5em;
		    line-height: 1.5em;
		   }
	  .graph .bar span { position: absolute; left: 1em; }">>)]).


%%%--------------------------------
%%% Update table schema and content from older versions
%%%--------------------------------

update_table() ->
		 case catch mnesia:table_info(presence_registered, size) of
		     Size when is_integer(Size) -> catch migrate_data_mod_presence(Size);
		     _ -> ok
		 end.

migrate_data_mod_presence(Size) ->
    Migrate = fun(Old, S) ->
		      {presence_registered, {US, _Host}, XML, Icon} = Old,
		      New = #webpresence{us = US,
					 ridurl = false,
					 jidurl = true,
					 xml = list_to_atom(XML),
					 avatar = false,
					 js = false,
					 text = false,
					 icon = Icon},
		      mnesia:write(New),
		      mnesia:delete_object(Old),
		      S-1
	      end,
    F = fun() -> mnesia:foldl(Migrate, Size, presence_registered) end,
    {atomic, 0} = mnesia:transaction(F),
    {atomic, ok} = mnesia:delete_table(presence_registered).
