%%%----------------------------------------------------------------------
%%% File    : mod_archive_webview.erl
%%% Author  : Olivier Goffart <ogoffart at kde dot org>
%%% Purpose : Online viewer of message archive.  (to be used with mod_archive_odbc)
%%% Created :
%%% Id      :
%%%----------------------------------------------------------------------

-module(mod_archive_webview).
-author('ogoffart@kde.org').

-export([
	 process/2
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl"). %for all the defines

-define(LINK(L) , "/archive/" ++ L).
%-define(P(Els), ?XE("p", Els)).
-define(PC(Text), ?XE("p", [?C(Text)])).
-define(PCT(Text), ?PC(?T(Text))).


-define(MYDEBUG(Format, Args),
        io:format("D(~p:~p:~p) : " ++ Format ++ "~n",
                  [calendar:local_time(), ?MODULE, ?LINE] ++ Args)).


%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

%process([], Request) -> process2([], Request, {}).

process(["style.css"], _) ->
    {200,[{"Content-Type", "text/css"}], "
#navigation li { list-style:none; display:inline;  }
.message_from, .message_to { margin:0; padding:0; }
.message_from .time { color:#BD2134; font-weight:bold; }
.message_from .jid  { color:#BD2134; font-weight:bold; }
.message_to   .time { color:#1E6CC6; font-weight:bold; }
.message_to   .jid  { color:#1E6CC6; font-weight:bold; }
.search_result a { display:block; }
.search_result em { display:block; color:green; }
/*a.link_prev { float:left } */
a.link_next { float:right }
.message_body { white-space: pre-wrap; }
"};

process(Path, #request{auth = Auth} = Request) ->
    ?MYDEBUG("Requested ~p ~p", [Path, Request]),
    
    case get_auth(Auth) of
        {User, Server} ->
            process2(Path, Request, {User, Server});
        unauthorized ->
            {401, [{"WWW-Authenticate", "basic realm=\"ejabberd-archives\""}],
                ejabberd_web:make_xhtml([{xmlelement, "h1", [],
                                               [{xmlcdata, "401 Unauthorized"}]}])}
    end.
   

%process2(["config" | tail], #request{lang = Lang } = Request , {User, Server}) ->

process2(["config" ], #request{lang = Lang } = _Request , {User, Server}) ->
    make_xhtml(?T("Config"),
        [?XE("h3", [?CT("Global Settings")]) ] ++ global_config_form({User, Server}, Lang) ++
        [?XE("h3", [?CT("Specific Contact Settings")]) ] ++ contact_config_form({User, Server}, Lang) ++
        [?X("hr"), ?ACT(?LINK("config/complex"),"Advanced settings")] , Lang);

process2(["config" , "submit", "global"], #request{q = Query } = Request , US) ->
    submit_config_global( Query  , US),
    process2(["config"], Request, US);

% process2(["config" , "submit", "contact"], #request{q = Query } = Request , US) ->
%     submit_config_contact( Query  , US),
%     process2(["config"], Request, US);

process2(["config" , "complex" ], #request{lang = Lang } = _Request , {User, Server}) ->
    make_xhtml(?T("Config"),
        [?XE("h3", [?CT("Global Settings")]) ] ++ global_config_form_complex({User, Server}, Lang) ++
        [?XE("h3", [?CT("Specific Contact Settings")]) ] ++ contact_config_form({User, Server}, Lang) ++
        [?X("hr"), ?ACT(?LINK("config"),"Simple settings")] , Lang);

process2(["config" , "submit", "complex_global"], #request{q = Query } = Request , US) ->
    submit_config_global_complex( Query  , US),
    process2(["config/complex"], Request, US);

process2(["contact"], #request{lang = Lang } = _Request , US) ->
    make_xhtml(?T("Contact List"), [
                           ?XE("ul", lists:map( fun({Node,Server,Count}) -> 
                                                    With = jlib:jid_to_string({Node,Server,""}),
                                                    ?LI([?AC(?LINK("contact/" ++ ejabberd_http:url_encode(With)), With ) ,
                                                         ?C(" (" ++ Count  ++")")] ) end,
                                                get_contacts(US)))
               ], Lang);

process2(["contact" , Jid], #request{lang = Lang } = _Request , US) ->
    make_xhtml(?T("Chat with ") ++ Jid, contact_config(Jid,US,Lang) ++
                           [?XE("ul", lists:map( fun({Id, Node, Server, Resource, Utc, Subject }) -> 
                                                    With = jlib:jid_to_string({Node,Server,Resource}),
                                                    ?LI([?AC(?LINK("show/" ++ integer_to_list(Id)), "On " ++ Utc ++ " with " ++ With ++ " -> " ++ escape_str(Subject)  )] ) end,
                                                get_collection_list(jlib:string_to_jid(Jid), US)))
               ], Lang);

process2(["show" , Id], #request{lang = Lang } = _Request , US) ->
    { With, Utc, Subject,  List, NPId } = get_collection(Id, US),
    [Date, _Time] = string:tokens(Utc, " "),
    make_xhtml(?T("Chat with ") ++ jlib:jid_to_string(With) ++ ?T(" on ") ++ Date ++ ?T(" : ") ++ escape_str(Subject),
               lists:map(fun(Msg) -> format_message(Msg,With, US) end, List)
               ++ links_previous_next(NPId, Lang)
               ++ [?X("hr"), ?XAE("form",[{"action",?LINK("edit/" ++ Id)},{"metohd","post"}],
                                  [?XE("label",[?CT("Edit subject: "),
                                                ?INPUT("text","subject",escape_str(Subject))]),
                                   ?INPUT("submit","submit",?T("Ok"))]),
                  ?XAE("form",[{"action",?LINK("delete/" ++ Id)},{"metohd","post"},
                               {"onsubmit","return confirm('"++ ?T("Do you realy want to delete this chat") ++"')"}],
                       [?INPUT("hidden","id",Id),?INPUT("submit","delete",?T("Delete"))])]
               , Lang);

process2(["edit" , Id], #request{ q = Query} = Request , US) ->
    case lists:keysearch("subject", 1, Query) of
        {value, {_, Subject}} -> change_subject(Id,Subject,US);
        _ -> ok
    end,
    process2(["show", Id] , Request, US);

process2(["delete" , Id], #request{q = Query, lang=Lang} = _Request , US) ->
    case lists:keysearch("id", 1, Query) of
        {value, {_, Id2}} when Id==Id2 -> 
            delete_collection(Id,US), make_xhtml("Chat deleted",[],Lang);
        _ -> ?ERR_INTERNAL_SERVER_ERROR
    end;

process2(["search"], #request{lang = Lang } = Request , US) ->
    make_xhtml(?T("Search"), [
                search_form(Request, US) ], Lang);

process2(["search", "results"], #request{lang = Lang } = Request , US) ->
    make_xhtml(?T("Search"), [ search_form(Request, US) | search_results(Request, US)], Lang);

process2([], #request{lang = Lang } = _Request , {LUser,LServer}) ->
    make_xhtml(?T("Archives viewer"),[?PCT("Welcome " ++ LUser ++ "@" ++ LServer)], Lang);

process2(_, #request{lang = Lang } = _Request , _US) ->
    make_xhtml(?T("404 File not found"),[], Lang).

%------------------------------

make_xhtml(Title, Els, Lang) ->
    {200, [html],
        {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"},
                {"xml:lang", Lang},
                {"lang", Lang}],
        [{xmlelement, "head", [],
        [?XE("title",  [?C(Title) , ?CT(" - ejabberd Web Archive Viewer")]),
        {xmlelement, "meta", [{"http-equiv", "Content-Type"},
                    {"content", "text/html; charset=utf-8"}], []},
        {xmlelement, "link", [{"href", ?LINK("style.css")},
                    {"type", "text/css"},
                    {"rel", "stylesheet"}], []}]},
        ?XE("body",
        [?XAE("div", [{"id", "container"}],
              [?XAE("div", [{"id", "header"}],
                    [?XE("h1", [?CT("Archives Viewer")])]),
               ?XAE("div", [{"id", "navigation"}],
                    [?XE("ul",
                     [?LI([?ACT(?LINK("config"), "Config")]), ?C(" "),
                      ?LI([?ACT(?LINK("contact"), "Browse")]), ?C(" "),
                      ?LI([?ACT(?LINK("search"), "Search")])])]), ?C(" "),
               ?XAE("div", [{"id", "content"}], [ ?XE("h2", [?C(Title)]) | Els])])])]}}.


get_auth(Auth) ->
    case Auth of
        {SJID, P} ->
            case jlib:string_to_jid(SJID) of
                error ->
                    unauthorized;
                #jid{user = U, server = S} ->
                    case ejabberd_auth:check_password(U, S, P) of
                        true ->
                            {U, S};
                        false ->
                            unauthorized
                    end
            end;
         _ ->
            unauthorized
    end.

select_element(Name, List, Value1) ->
    Value = if is_integer(Value1) -> integer_to_list(Value1); true -> Value1 end,
    ?XAE("select",[{"name",Name}],lists:map( 
        fun({Key,Text}) -> ?XAE("option", 
                            case Key of
                                Value -> [{"value",Value},{"selected","selected"}];
                                _ -> [{"value",Key}]
                            end, [?C(Text)]) end, List)).

table_element(Rows) ->
    ?XE("table",lists:map(fun(Cols)-> ?XE("tr", lists:map(fun(Ct)-> ?XE("td",Ct) end, Cols)) end, Rows)).

%------------------------

format_message({ Utc, Dir, Body } ,{WithU,WithS,WithR}, {LUser,LServer} ) ->
    {From, Class} = case Dir of 
        0 -> { jlib:jid_to_string({WithU,WithS,WithR}) , "message_from" } ;
        1 -> { jlib:jid_to_string({LUser,LServer,""}) , "message_to" } 
    end,
    [_Date, Time] = string:tokens(Utc, " "),
    ?XAE("p", [{"class", Class}] , [ ?XAE("span", [{"class","time"}], [?C("["++Time++"]")]), ?C(" "),
                                   ?XAE("span", [{"class","jid"}], [?C(From)]), ?C(": "),
                                   ?XAE("span", [{"class","message_body"}], [?C(Body)])]).

contact_config(Jid,{LUser,LServer},Lang) ->
    %run_sql_transaction(LServer, fun() -> run_sql_query("") end)
    %[?XE("p",[?CT("Automatic archive with this contact is " + Au   ), ].
    [].


global_config_form({LUser,LServer},Lang) ->
    {Save,Expire,Auto_save} =
        case run_sql_transaction(LServer, fun() -> run_sql_query(
                "SELECT save,expire,auto_save"
                " FROM archive_global_prefs"
                " WHERE us = " ++ get_us_escaped({LUser,LServer}) ) end) of
            {selected, _ , [ Ok ]} -> Ok;
            {selected, _ , [ ]} -> { -1, -1, -1 }
        end,
   [?XAE("form",[{"action",?LINK("config/submit/global")}],
         [?XE("label",[?CT("Disable or enable automatic archiving globaly: "), select_element("global_auto_save",
                        [{"-1",?T("--Server Default--")},{"1",?T("Enabled")},{"0",?T("Disabled")}],decode_integer(Auto_save))]),
          ?BR,
          ?XE("label",[?CT("Default for contact not specified bellow : "), select_element("global_save",
                             [{"-1",?T("--Server Default--")},{"1",?T("Enabled")},{"0",?T("Disabled")}],decode_integer(Save))]),
          ?BR, ?XE("label",[?CT("Default expiration time: "), ?INPUT("text","global_expire",integer_to_list(decode_integer(Expire)))]),
               ?CT("(number of seconds before deleting message, '-1' = server default)"),
          ?BR, ?INPUTT("submit","global_submit","Submit")]
         )].

global_config_form_complex({LUser,LServer},Lang) ->
    {Save,Expire,Otr,Method_auto,Method_local,Method_manual,Auto_save} =
        case run_sql_transaction(LServer, fun() -> run_sql_query(
                "SELECT save,expire,otr,method_auto,method_local,method_manual,auto_save"
                " FROM archive_global_prefs"
                " WHERE us = " ++ get_us_escaped({LUser,LServer}) ) end) of
            {selected, _ , [ Ok ]} -> Ok;
            {selected, _ , [ ]} -> { -1, -1, -1, -1, -1, -1, -1 }
        end,
    MethodList = [ {"-1",?T("--Undefined--")}, {"0",?T("Prefer")}, {"1",?T("Concede")}, {"2",?T("Forbid")} ],
    [?XAE("form",[{"action",?LINK("config/submit/complex_global")}],[table_element([[
            [?XE("label",[?CT("Save: "), select_element("global_save",[{"-1",?T("--Default--")},{"1",?T("Enabled")},{"0",?T("Disabled")}],decode_integer(Save))])],
            [?XE("label",[?CT("Expire: "), ?INPUT("text","global_expire",integer_to_list(decode_integer(Expire)))])],
            [?XE("label",[?CT("Otr: "), select_element("global_otr",[{"-1",?T("--Undefined--")},
                                                                      {"0",?T("Approve")},
                                                                      {"1",?T("Concede")},
                                                                      {"2",?T("Forbid")},
                                                                      {"3",?T("Oppose")},
                                                                      {"4",?T("Prefer")},
                                                                      {"5",?T("Require")} ],decode_integer(Otr))])],
            [?XE("label",[?CT("Auto Method: "), select_element("global_method_auto", MethodList,decode_integer(Method_auto))])],
            [?XE("label",[?CT("Local Method: "), select_element("global_method_local", MethodList,decode_integer(Method_local))])],
            [?XE("label",[?CT("Manual Method: "), select_element("global_method_manual", MethodList,decode_integer(Method_manual))])],
            [?XE("label",[?CT("Auto Save "), select_element("global_auto_save",
                                [{"-1",?T("--Default--")},{"1",?T("Enabled")},{"0",?T("Disabled")}],decode_integer(Auto_save))])],
            [?INPUT("submit","global_modify",?T("Modify"))]
       ]])])].

    
contact_config_form({LUser,LServer},Lang) ->
     {selected, _, List} =
          run_sql_transaction(LServer, fun() -> run_sql_query(
             "SELECT with_user,with_server,with_resource,save,expire"
             " FROM archive_jid_prefs"
             " WHERE us = " ++ get_us_escaped({LUser,LServer}) ) end),
    [ table_element([[[?CT("JID")],[?CT("Auto archive")],[?CT("Expire")]] |
        lists:map(fun({WithU,WithS,WithR,Save,Expire}) -> 
            [ [?C(jlib:jid_to_string({WithU,WithS,WithR}))],
              [case decode_integer(Save) of 1 -> ?CT("Enabled"); 0 -> ?CT("Disabled"); _ -> ?CT("Default") end],
              [?C(integer_to_list(decode_integer(Expire)))]  ] end , List ) ]) %,
%       ?XAE("form",[{"action",?LINK("config/submit/contact")}],
%          [?XE("label",[?CT("Add/Modify settings for Jid: "), ?INPUT("text","jid","")]),
%           ?BR,
%           ?XE("label",[?CT("Archiving : "), select_element("save",
%                              [{"-1",?T("--Default--")},{"1",?T("Enabled")},{"0",?T("Disabled")}],"-1")]),
%           ?BR, ?XE("label",[?CT("Expiration time: "), ?INPUT("text","expire","-1")]),
%           ?BR, ?INPUTT("submit","submit","Submit")]
%          )
    ].



get_from_query_escaped(Key,Query) ->
    {value, {_, Value}} = lists:keysearch(Key, 1, Query),
    case Value of
        -1 -> "NULL";
        Integer when is_integer(Integer) -> Integer;
        "-1" -> "NULL";
        Value -> "'" ++ ejabberd_odbc:escape(Value) ++ "'"
    end.

submit_config_global(Query , {LUser,LServer}) ->
    SUS = get_us_escaped({LUser,LServer}),
    SQLQuery = 
        "UPDATE archive_global_prefs"
        " SET save = " ++ get_from_query_escaped("global_save",Query) ++ ","
        "     expire = " ++ get_from_query_escaped("global_expire",Query) ++ ","
        "     auto_save = " ++ get_from_query_escaped("global_auto_save",Query) ++ 
        " WHERE us = " ++ SUS,
    F = fun() ->
        %TODO: use REPLACE
        case run_sql_query("SELECT us FROM archive_global_prefs WHERE us = " ++ SUS) of
            {selected, _, Rs} when Rs /= [] -> ok;
            _ -> run_sql_query("INSERT INTO archive_global_prefs (us) VALUES (" ++ SUS ++ ")")
        end,
        run_sql_query(SQLQuery) end,
    run_sql_transaction(LServer, F).

submit_config_global_complex(Query , {LUser,LServer}) ->
    SUS = get_us_escaped({LUser,LServer}),
    SQLQuery = 
        "UPDATE archive_global_prefs"
        " SET save = " ++ get_from_query_escaped("global_save",Query) ++ ","
        "     expire = " ++ get_from_query_escaped("global_expire",Query) ++ ","
        "     otr = " ++ get_from_query_escaped("global_otr",Query) ++ ","
        "     method_auto = " ++ get_from_query_escaped("global_method_auto",Query) ++ ","
        "     method_local = " ++ get_from_query_escaped("global_method_local",Query) ++ ","
        "     method_manual = " ++ get_from_query_escaped("global_method_manual",Query) ++ ","
        "     auto_save = " ++ get_from_query_escaped("global_auto_save",Query) ++ 
        " WHERE us = " ++ SUS,
    F = fun() ->
        %TODO: use REPLACE
        case run_sql_query("SELECT us FROM archive_global_prefs WHERE us = " ++ SUS) of
            {selected, _, Rs} when Rs /= [] -> ok;
            _ -> run_sql_query("INSERT INTO archive_global_prefs (us) VALUES (" ++ SUS ++ ")")
        end,
        run_sql_query(SQLQuery) end,
    run_sql_transaction(LServer, F).
    
get_from_query_with_default(Key,Query,Default) ->
    case  lists:keysearch(Key, 1, Query) of
        {value, {_, Value}} -> Value;
        _ -> Default
    end.

search_form( #request{lang = Lang, q = Query } = _Request, US) ->
    ?XAE("form",[{"method","post"},{"action", ?LINK("search/results")}],
          [ ?XE("label", [?CT("With: ") ,
                select_element("with" , [ { "", "--All--" } |
                                            lists:map( fun({Node,Server,_Count}) -> 
                                                    With = jlib:jid_to_string({Node,Server,""}),
                                                    {With, With}  end,
                                                    get_contacts(US)) ],
                                get_from_query_with_default("with",Query,""))]),
            ?BR,
            ?XE("label", [?CT("From: ") , ?INPUT("text","from", get_from_query_with_default("from",Query,"")),
                          ?CT(" (date in SQL format,  may be empty)")]),
            ?BR,
            ?XE("label", [?CT("To: ") , ?INPUT("text","to", get_from_query_with_default("to",Query,"")),
                          ?CT(" (date in SQL format,  may be empty)")]),
            ?BR,
            ?XE("label", [?CT("Search keyword: ") , ?INPUT("text","keywords",
                                                        get_from_query_with_default("keywords",Query,""))]),
            ?BR,
            ?INPUT("submit","search",?T("Search"))  ]).



search_results( #request{lang = Lang, q = Query } = _Request, {_, LServer} = US) ->
    With = case  lists:keysearch("with", 1, Query) of
        {value, {_, Value}} -> case jlib:string_to_jid(Value) of
            #jid{ luser = Node , lserver = Server , lresource = "" } -> 
                " AND with_user ='" ++ ejabberd_odbc:escape(Node) ++ "'" ++
                " AND with_server ='" ++ ejabberd_odbc:escape(Server) ++ "'";
            #jid{ luser = Node , lserver = Server , lresource = Resource } -> 
                " AND with_user ='" ++ ejabberd_odbc:escape(Node) ++ "'" ++
                " AND with_server ='" ++ ejabberd_odbc:escape(Server) ++ "'" ++
                " AND with_resource ='" ++ ejabberd_odbc:escape(Resource) ++ "'";
            _ -> ""
            end;
        _ -> ""
    end,
    From = case lists:keysearch("from", 1, Query) of
        {value, {_, V1}} when V1 /= "" -> " AND M.utc >= '" ++ ejabberd_odbc:escape(V1) ++ "'";
        _ -> ""
    end,
    To = case lists:keysearch("to", 1, Query) of
        {value, {_, V2}} when V2 /= "" -> " AND M.utc <= '" ++ ejabberd_odbc:escape(V2) ++ "'";
        _ -> ""
    end,
    Kw = case lists:keysearch("keywords", 1, Query) of
        {value, {_, V3}} when V3 /= "" -> " AND body LIKE '%" ++ ejabberd_odbc:escape(V3) ++ "%'";
        _ -> ""
    end,
    
    F = fun() -> run_sql_query(
            "SELECT coll_id,subject,with_user,with_server,with_resource,C.utc,body"
            " FROM archive_collections as C, archive_messages as M"
            " WHERE C.id = M.coll_id AND C.us = " ++  get_us_escaped(US) ++ 
            "   AND C.deleted='0'" ++ With ++ From ++ To ++ Kw ++
            " GROUP BY coll_id") end,
    case run_sql_transaction(LServer,F) of
        {selected, _ , []} -> [?PCT("No matches")];
        {selected, _ , Results} ->
           lists:map(fun(R) -> format_search_result(R,Lang) end, Results)
    end.

format_search_result( {Id,Subject,User,Server,Resource,Utc,Body} ,_Lang) ->
    ?XAE("p",[{"class","search_result"}],
         [?AC(?LINK("show/" ++ integer_to_list(Id)), jlib:jid_to_string({User,Server,Resource}) ++ " : " ++ escape_str(Subject)),
          ?C(Body), ?XE("em",[?C(Utc)]) ] ).
          
links_previous_next({PrevId,NextId},Lang) ->
    [?XAE("p",[{"class","links_previous_next"}],
        links_previous_next_aux("link_prev", ?T("Previous"), PrevId) ++ [?C(" ")] ++
        links_previous_next_aux("link_next", ?T("Next"), NextId))].

links_previous_next_aux(Class, Text, Id) ->
    case Id of
        -1 -> [];
        _ -> [?XAE("a",[{"href",?LINK("show/" ++ integer_to_list(Id))},{"class",Class}], [?C(Text)])]
    end.

%------------------------

get_contacts({LUser, LServer}) ->
    Fun = fun() ->
        {selected, _ , Contacts} = run_sql_query("SELECT with_user,with_server,COUNT(*)"
                                                 " FROM archive_collections"
                                                 " WHERE us = " ++ get_us_escaped({LUser,LServer}) ++ " AND deleted=0"
                                                 " GROUP BY with_user,with_server"),
        Contacts end,
    run_sql_transaction(LServer, Fun).

get_collection_list(Jid, {LUser, LServer}) ->
    {WithU, WithS, _} = get_jid_escaped(Jid),
    Fun = fun() ->
        {selected, _ , List} = run_sql_query("SELECT id,with_user,with_server,with_resource,utc,subject"
                                                 " FROM archive_collections"
                                                 " WHERE us = " ++ get_us_escaped({LUser,LServer}) ++ 
                                                 "  AND deleted=0 "
                                                 "  AND with_user = " ++ WithU ++
                                                 "  AND with_server = " ++ WithS),
        List end,
    run_sql_transaction(LServer, Fun).
    
get_collection(Id,{LUser,LServer}) ->
    Fun = fun() ->
        SUS = get_us_escaped({LUser,LServer}),
        {selected, _ , [{WithU, WithS, WithR, Utc, Subject}] } = run_sql_query(
                            "SELECT with_user,with_server,with_resource,utc,subject"
                            " FROM archive_collections"
                            " WHERE id = '" ++ ejabberd_odbc:escape(Id) ++ "'" 
                            "  AND us = " ++ SUS),
        %If the previous query fail, that mean the collection doesn't exist or is not 
        % one of the users connection.

        {selected, _ , List} = run_sql_query("SELECT utc,dir,body"
                                                 " FROM archive_messages"
                                                 " WHERE coll_id = '" ++ ejabberd_odbc:escape(Id) ++ "'"),
        NextId = case run_sql_query("SELECT id"
                                    " FROM archive_collections"
                                    " WHERE us = " ++  SUS ++  " AND deleted=0 "
                                    "  AND with_user ='" ++ ejabberd_odbc:escape(WithU) ++ "'" ++
                                    "  AND with_server ='" ++ ejabberd_odbc:escape(WithS) ++ "'" ++
                                    "  AND utc > '" ++ Utc ++ "'" ++
                                    " ORDER BY utc LIMIT 1") of
            {selected, _ , [{V1}]} -> V1;
            _ -> -1
        end,
        PrevId = case run_sql_query("SELECT id"
                                    " FROM archive_collections"
                                    " WHERE us = " ++  SUS ++  " AND deleted=0 "
                                    "  AND with_user ='" ++ ejabberd_odbc:escape(WithU) ++ "'" ++
                                    "  AND with_server ='" ++ ejabberd_odbc:escape(WithS) ++ "'" ++
                                    "  AND utc < '" ++ Utc ++ "'" ++
                                    " ORDER BY utc DESC LIMIT 1") of
            {selected, _ , [{V2}]} -> V2;
            _ -> -1
        end,
        { {WithU,WithS,WithR} , Utc,  Subject , List, {PrevId,NextId}} end,
    run_sql_transaction(LServer, Fun).

change_subject(Id,Subject,{LUser,LServer}) ->
    run_sql_transaction(LServer, fun() -> run_sql_query(
            "UPDATE archive_collections"
            " SET subject='"++ ejabberd_odbc:escape(Subject)++"',"
            "     change_utc=NOW(), change_by='webview'"
            " WHERE id = '" ++ ejabberd_odbc:escape(Id) ++ "'" 
            "  AND us = " ++ get_us_escaped({LUser,LServer})) end).

delete_collection(Id,{LUser,LServer}) ->
    run_sql_transaction(LServer, fun() -> run_sql_query(
            "UPDATE archive_collections"
            " SET deleted=1, subject='', thread = '', extra = '', prev_id = NULL, next_id = NULL,"
            "     change_utc=NOW(), change_by='webview'"
            " WHERE id = '" ++ ejabberd_odbc:escape(Id) ++ "'" 
            "  AND us = " ++ get_us_escaped({LUser,LServer})) end).

%------------------------
% from mod_archive_odbc
run_sql_query(Query) ->
    ?MYDEBUG("running query: ~p", [lists:flatten(Query)]),
    case catch ejabberd_odbc:sql_query_t(Query) of
        {'EXIT', Err} ->
            ?ERROR_MSG("unhandled exception during query: ~p", [Err]),
            exit(Err);
        {error, Err} ->
            ?ERROR_MSG("error during query: ~p", [Err]),
            throw({error, Err});
        aborted ->
            ?ERROR_MSG("query aborted ~p", [Query]),
            throw(aborted);
        R -> ?MYDEBUG("query result: ~p", [R]),
            R
    end.

run_sql_transaction(LServer, F) ->
    DBHost = gen_mod:get_module_opt(LServer, ?MODULE, db_host, LServer),
    case ejabberd_odbc:sql_transaction(DBHost, F) of
        {atomic, R} ->
            ?MYDEBUG("succeeded transaction: ~p", [R]),
            R;
        {error, Err} -> {error, Err};
        E ->
            ?ERROR_MSG("failed transaction: ~p, stack: ~p", [E, process_info(self(),backtrace)]),
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.
    
get_us_escaped({LUser, LServer}) ->
    "'" ++ ejabberd_odbc:escape(LUser ++ "@" ++ LServer) ++ "'".

get_jid_escaped({LUser, LServer, LResource}) ->
    {"'" ++ ejabberd_odbc:escape(LUser), "'" ++ ejabberd_odbc:escape(LServer),
     "'" ++ ejabberd_odbc:escape(LResource)};

get_jid_escaped(#jid{luser = LUser, lserver = LServer, lresource=LResource}) ->
    {"'" ++ ejabberd_odbc:escape(LUser) ++ "'", "'" ++ ejabberd_odbc:escape(LServer) ++ "'",
     "'" ++ ejabberd_odbc:escape(LResource) ++ "'"}.

decode_integer(Val) when is_integer(Val) ->
    Val;
decode_integer(null) ->
    -1;
decode_integer(Val) ->
    list_to_integer(Val).

escape_str(null) -> "";
escape_str(Str) -> Str.
