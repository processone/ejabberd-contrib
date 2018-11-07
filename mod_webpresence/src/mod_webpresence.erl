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
-export([start/2,
         stop/1,
         remove_user/2,
         web_menu_host/3, web_page_host/3,
         process_disco_info/1,
         process_disco_items/1,
         process_vcard/1,
         process_register/1,
         process/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,
	 mod_opt_type/1, mod_options/1, depends/2]).

%% API
-export([start_link/0]).

-include("xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_web_admin.hrl").
-include("ejabberd_http.hrl").

-record(webpresence, {us, ridurl = false, jidurl = false, xml = false, avatar = false, js = false, text = false, icon = "jsf-text"}).
-record(state, {host, server_host, base_url, access}).
-record(presence2, {resource, show, priority, status}).

%% Copied from ejabberd_sm.erl
-record(session, {sid, usr, us, priority, info}).

-define(PIXMAPS_DIR, <<"pixmaps">>).
-define(AUTO_ACL, webpresence_auto).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start(Host, Opts) ->
    Default_dir = case code:priv_dir(ejabberd) of
		      {error, _} -> ?PIXMAPS_DIR;
		      Path -> filename:join([Path, ?PIXMAPS_DIR])
		  end,
    Dir = gen_mod:get_opt(pixmaps_path, Opts, fun(D) -> D end, Default_dir),
    catch ets:new(pixmaps_dirs, [named_table, public]),
    ets:insert(pixmaps_dirs, {directory, Dir}),
    case gen_mod:start_child(?MODULE, Host, Opts) of
       {ok, Ref} ->
           {ok, Ref};
       {error, {already_started, Ref}} ->
           {ok, Ref};
       {error, Reason} ->
           {error, Reason}
    end.

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    gen_mod:stop_child(?MODULE, Host),
    ok.

-spec mod_opt_type(atom()) -> fun((term()) -> term()).
mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(access) -> fun acl:access_rules_validator/1;
mod_opt_type(pixmaps_path) -> fun iolist_to_binary/1;
mod_opt_type(port) ->
    fun(I) when is_integer(I), I>0, I<65536 -> I end;
mod_opt_type(path) -> fun iolist_to_binary/1;
mod_opt_type(baseurl) -> fun iolist_to_binary/1.

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(Host) ->
    [{host, <<"webpresence.@HOST@">>},
     {access, none},
     {pixmaps_path, ?PIXMAPS_DIR},
     {port, 5280},
     {path, <<"presence">>},
     {baseurl, iolist_to_binary(io_lib:format(<<"http://~s:5280/presence/">>, [Host]))}].

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

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
    BaseURL1 = gen_mod:get_opt(baseurl, Opts, fun(O) -> O end,
                               iolist_to_binary(io_lib:format(<<"http://~s:~p/~s/">>, [Host, Port, Path]))),
    BaseURL2 = ejabberd_regexp:greplace(BaseURL1, <<"@HOST@">>, Host),
    register_iq_handlers(MyHost),
    ejabberd_router:register_route(MyHost, Host),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(webadmin_menu_host, Host, ?MODULE, web_menu_host, 50),
    ejabberd_hooks:add(webadmin_page_host, Host, ?MODULE, web_page_host, 50),
    {ok, #state{host = MyHost,
		server_host = Host,
		base_url = BaseURL2,
		access = Access}}.

register_iq_handlers(Host) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_REGISTER,
                                  ?MODULE, process_register),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VCARD,
                                  ?MODULE, process_vcard),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO,
                                  ?MODULE, process_disco_info),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS,
                                  ?MODULE, process_disco_items).

unregister_iq_handlers(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS).

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
handle_info({route, Packet},
	    #state{host = Host,
		   server_host = ServerHost,
		   base_url = BaseURL,
		   access = Access} = State) ->
    From = xmpp:get_from(Packet),
    To = xmpp:get_to(Packet),
    case catch do_route(Host, ServerHost, Access, From, To, Packet, BaseURL) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ok
    end,
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
    unregister_iq_handlers(Host),
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
            Lang = xmpp:get_lang(Packet),
	    ErrText = <<"Access denied by service policy">>,
	    Err = xmpp:err_forbidden(ErrText, Lang),
	    ejabberd_router:route_error(Packet, Err)
    end.

-spec process_vcard(iq()) -> iq().
process_vcard(#iq{type = get, lang = Lang, sub_els = [#vcard_temp{}]} = IQ) ->
    Desc = translate:translate(Lang, <<"ejabberd Web Presence module">>),
    xmpp:make_iq_result(
      IQ, #vcard_temp{fn = <<"ejabberd/mod_webpresence">>,
                      url = ejabberd_config:get_uri(),
                      desc = Desc});
process_vcard(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_vcard(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

do_route1(_Host, _From, _To, #iq{} = IQ, _BaseURL) ->
    ejabberd_router:process_iq(IQ);
do_route1(_Host, _From, _To, Packet, _BaseURL) ->
    case xmpp:get_type(Packet) of
	error -> ok;
	result -> ok;
	_ ->
	    Err = xmpp:err_item_not_found(),
	    ejabberd_router:route_error(Packet, Err)
    end.

-spec process_register(iq()) -> iq().
process_register(#iq{type = get, from = From, to = To, lang = Lang,
                     sub_els = [#register{}]} = IQ) ->
    Host = To#jid.lserver,
    xmpp:make_iq_result(IQ, iq_get_register_info(Host, From, Lang));
process_register(#iq{type = set, from = From, to = To,
                     lang = Lang, sub_els = [El = #register{}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    case process_iq_register_set(ServerHost, Host, From, El, Lang) of
        {result, Result} ->
            xmpp:make_iq_result(IQ, Result);
        {error, Err} ->
            xmpp:make_error(IQ, Err)
    end.


-spec process_disco_info(iq()) -> iq().
process_disco_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_info(#iq{type = get, lang = Lang,
                       sub_els = [#disco_info{node = <<"">>}]} = IQ) ->
    Features = [?NS_DISCO_INFO, ?NS_DISCO_ITEMS,
                ?NS_REGISTER, ?NS_VCARD],
    Identity = #identity{category = <<"component">>,
                         type = <<"presence">>,
                         name = translate:translate(Lang, <<"Web Presence">>)},
    xmpp:make_iq_result(
      IQ, #disco_info{features = Features,
                      identities = [Identity]});
process_disco_info(#iq{type = get, lang = Lang,
                       sub_els = [#disco_info{}]} = IQ) ->
    xmpp:make_error(IQ, xmpp:err_item_not_found(<<"Node not found">>, Lang));
process_disco_info(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_disco_items(iq()) -> iq().
process_disco_items(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_items(#iq{type = get,
                        sub_els = [#disco_items{}]} = IQ) ->
    xmpp:make_iq_result(IQ, #disco_items{});
process_disco_items(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

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

get_pr(LUS) ->
    case catch mnesia:dirty_read(webpresence, LUS) of
       [#webpresence{jidurl = J, ridurl = H, xml = X, avatar = A, js = S, text = T, icon = I}] ->
           {J, H, X, A, S, T, I, true};
       _ ->
           {true, false, false, false, false, false, <<"">>, false}
    end.


iq_get_register_info(Host, From, Lang) ->
    LUS = {From#jid.luser, From#jid.lserver},
    {_JidUrl, _RidUrl, _XML, _Avatar, _JS, _Text, Icon, Registered} = get_pr(LUS),
    Nick = Icon,
    Title = <<(translate:translate(
                 Lang, <<"Web Presence Registration at ">>))/binary, Host/binary>>,
    Inst = translate:translate(Lang, <<"Enter iconset you want to use by default">>),
    Fields = muc_register:encode([{roomnick, Nick}], Lang),
    X = #xdata{type = form, title = Title,
               instructions = [Inst], fields = Fields},
    #register{nick = Nick,
              registered = Registered,
              instructions =
                  translate:translate(
                    Lang, <<"You need a client that supports x:data "
                            "to register to Web Presence">>),
              xdata = X}.

process_iq_register_set(_ServerHost, Host, From,
                        #register{remove = true}, Lang) ->
    unregister_webpresence(From, Host, Lang);
process_iq_register_set(_ServerHost, _Host, _From,
                        #register{xdata = #xdata{type = cancel}}, _Lang) ->
    {result, undefined};
process_iq_register_set(ServerHost, Host, From,
                        #register{nick = Nick, xdata = XData}, Lang) ->
    case XData of
        #xdata{type = submit, fields = Fs} ->
            try
                Options = muc_register:decode(Fs),
                N = proplists:get_value(roomnick, Options),
                iq_set_register_info(ServerHost, Host, From, N, Lang)
            catch _:{muc_register, Why} ->
                    ErrText = muc_register:format_error(Why),
                    {error, xmpp:err_bad_request(ErrText, Lang)}
            end;
        #xdata{} ->
            Txt = <<"Incorrect data form">>,
            {error, xmpp:err_bad_request(Txt, Lang)};
        _ when is_binary(Nick), Nick /= <<"">> ->
            iq_set_register_info(ServerHost, Host, From, Nick, Lang);
        _ ->
            ErrText = <<"You must fill in field \"Nickname\" in the form">>,
            {error, xmpp:err_not_acceptable(ErrText, Lang)}
    end.

iq_set_register_info(ServerHost, Host, From, Nick,
                     Lang) ->
    case iq_set_register_info2(ServerHost, Host, From, Nick, Lang) of
      {atomic, ok} -> {result, undefined};
      {atomic, false} ->
          ErrText = <<"That nickname is registered by another "
                      "person">>,
          {error, xmpp:err_conflict(ErrText, Lang)};
      _ ->
          Txt = <<"Database failure">>,
          {error, xmpp:err_internal_server_error(Txt, Lang)}
    end.

iq_set_register_info2(ServerHost, Host, From, Icon, Lang) ->
    %% RidUrl2 = get_rid_final_value(RidUrl, LUS),
    LUS = {From#jid.luser, From#jid.lserver},
    WP = #webpresence{us = LUS,
		      jidurl = true,
		      ridurl = false,
		      xml = true,
		      avatar = true,
		      js = true,
		      text = true,
		      icon = Icon},
    F = fun() -> mnesia:write(WP) end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    BaseURL = get_baseurl(ServerHost),
	    send_message_registered(WP, From, Host, BaseURL, Lang),
	    {atomic, ok};
	_ ->
	    {error, xmpp:err_internal_server_error()}
    end.

send_message_registered(WP, To, Host, BaseURL, Lang) ->
    {User, Server} = WP#webpresence.us,
    JIDS = jid:encode({User, Server, <<"">>}),
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
      jid:make(<<"">>, Host, <<"">>),
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

unregister_webpresence(From, Host, Lang) ->
    remove_user(From#jid.luser, From#jid.lserver),
    send_message_unregistered(From, Host, Lang),
    {result, undefined}.

remove_user(User, Server) ->
    mnesia:dirty_delete(webpresence, {User, Server}).

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
    From = jid:make(LUser, LServer, <<"">>),
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
			 icon = "jsf-jabber-text"}
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

session_to_presence(#session{sid = {_, Pid}}) ->
    P = ejabberd_c2s:get_presence(Pid),
    #presence2{resource = (P#presence.from)#jid.resource,
              show = misc:atom_to_binary(humanize_show(P#presence.show)),
              priority = P#presence.priority,
              status = xmpp:get_text(P#presence.status)}.

humanize_show(undefined) ->
    available;
humanize_show(Show) ->
    Show.

get_presences({bare, LUser, LServer}) ->
    [session_to_presence(Session) ||
        Session <- mnesia:dirty_index_read(session, {LUser, LServer}, #session.us)];

get_presences({sorted, LUser, LServer}) ->
    lists:sort(
      fun(A, B) ->
              if
                  A#presence2.priority == B#presence2.priority ->
                      WA = get_status_weight(A#presence2.show),
                      WB = get_status_weight(B#presence2.show),
                      WA < WB;
                  true ->
                      A#presence2.priority > B#presence2.priority
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
                                        {<<"name">>, Presence#presence2.resource},
                                        {<<"show">>, Presence#presence2.show},
                                        {<<"priority">>, intund2string(Presence#presence2.priority)}
                                       ],
                               children = [{xmlcdata, Presence#presence2.status}]
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
	    R#presence2.status
    end;

get_presences({status, LUser, LServer}) ->
    case get_presences({sorted, LUser, LServer}) of
        [Highest | _Rest] ->
            Highest#presence2.status;
        _ ->
            <<"unavailable">>
    end;

get_presences({show, LUser, LServer, LResource}) ->
    case get_presences({sorted, LUser, LServer}) of
	[] -> <<"unavailable">>;
	Rs ->
	    {value, R} = lists:keysearch(LResource, 2, Rs),
	    R#presence2.show
    end;

get_presences({show, LUser, LServer}) ->
    case get_presences({sorted, LUser, LServer}) of
        [Highest | _Rest] ->
            Highest#presence2.show;
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
				       Show =  Pr#presence2.show,
				       ?BC([<<"{name:'">>, Pr#presence2.resource, <<"',\n">>,
					   <<" priority:">>, intund2string(Pr#presence2.priority), <<",\n">>,
					   <<" show:'">>, Show, <<"',\n">>,
					   <<" long_show:'">>, long_show(Show, Lang), <<"',\n">>,
					   <<" status:'">>, escape(Pr#presence2.status), <<"',\n">>,
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

intund2string(undefined) -> intund2string(0);
intund2string(Int) when is_integer(Int) -> list_to_binary(integer_to_list(Int)).

escape(S1) ->
    S2 = re:replace(S1, "\'", "\\'", [global, {return, list}]),
    re:replace(S2, "\n", "\\n", [global, {return, list}]).

get_baseurl(Host) ->
    BaseURL1 = gen_mod:get_module_opt(Host, ?MODULE, baseurl),
    ejabberd_regexp:greplace(BaseURL1, <<"@HOST@">>, Host).

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
    Presence_xml = fxml:element_to_binary(get_presences({xml, LUser, LServer, Show_us})),
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
    [{_, Module, Function}] = ets:lookup(ejabberd_sm, {LServer, ?NS_VCARD}),
    JID = jid:make(LUser, LServer, <<"">>),
    IQ = #iq{type = get, from = JID, to = JID},
    IQr = Module:Function(IQ),
    [VCard] = IQr#iq.sub_els,
    Mime = fxml:get_path_s(VCard, [{elem, <<"PHOTO">>}, {elem, <<"TYPE">>}, cdata]),
    BinVal = fxml:get_path_s(VCard, [{elem, <<"PHOTO">>}, {elem, <<"BINVAL">>}, cdata]),
    Photo = misc:decode_base64(BinVal),
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
    LServer = jid:nameprep(Server),
    true = lists:member(Server, ejabberd_config:get_myhosts()),
    LUser = jid:nodeprep(User),
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
