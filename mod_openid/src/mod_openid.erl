
%%%----------------------------------------------------------------------
%%% File    : mod_openid.erl
%%% Author  : Olivier Goffart <ogoffart@kde.org>
%%% Purpose : Open id provider using XEP-0070
%%% Created : 24 Dec 2007 Olivier Goffart
%%%----------------------------------------------------------------------
%%% Copyright (c) 2007-2008 Olivier Goffart
%%%----------------------------------------------------------------------


%% WARNING: secret/1 and new_assoc/2 MUST be implemented correctly for security issue

-module(mod_openid).
-author('ogoffart@kde.org').

%% External exports
-export([process/2]).


-include("ejabberd.hrl").
-include("jlib.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

-record(profile, {identity, server, lang, jid}).

-define(MYDEBUG(Format,Args),io:format("D(~p:~p:~p) : "++Format++"~n",
				       [calendar:local_time(),?MODULE,?LINE]++Args)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PART 1 : OpenID

process([Jid],  #request{ q = Query, lang = Lang} = Request) ->
    %%?MYDEBUG("Auth Failed ~p ~n", [C]),
    %%[{server, Server}] = ets:lookup(mod_openid, server)
    JJid = jlib:string_to_jid(Jid),
    Server = "http://" ++ JJid#jid.server ++ ":5280/openid",
    Profile = #profile{identity = Server  ++"/"++ Jid,
		       server = Server ++ "/" ++ Jid,
		       lang = Lang, jid= JJid},
    case lists:keysearch("openid.mode", 1, Query) of
	{value, {_, "associate"}}  -> associate(Query,Profile);
	{value, {_, "checkid_immediate"}}  -> checkid_immediate(Query,Profile);
	{value, {_, "checkid_setup"}}  -> checkid_setup(Request,Profile);
	{value, {_, "check_authentication"}}  -> check_authentication(Query,Profile);
	_ -> default_page(Profile)
    end;

process(_,  _) ->error_400("Invalid identity").

default_page(Profile) ->
    make_xhtml([{xmlelement,"h1",[],[{xmlcdata, "400 Bad Request"}]}],Profile).

associate(_,Profile) ->
    not_implemented(Profile).

checkid_immediate(_,Profile) ->
    not_implemented(Profile).

checkid_setup(#request{ q = Query} = Request, Profile) ->
    case lists:keysearch("openid.return_to", 1, Query) of
	{value, {_, ReturnTo}} ->
	    case catch verify_id(Request,Profile) of
		verified -> checkid2(Request,Profile,ReturnTo);
		{return, Value} -> Value;
		_ -> redirect_reply([{"openid_mode","error"},
				     {"openid_error","InternalError"}], ReturnTo)
	    end;
	_ -> error_400("Missing 'openid.return_to'")
    end.

%% Helper for checkid_setup
checkid2(#request{ q = Query} = _Request, Profile,ReturnTo) ->
    case lists:keysearch("openid.identity", 1, Query) of
	{value, {_, Ident}} when Ident ==  Profile#profile.identity ->
	    {AssocHandle,Secret} =
		case lists:keysearch("openid.assoc_handle", 1, Query) of
		    {value, {_, V}} -> case secret(V) of
					   {ok, S} -> {V,S};
					   false -> new_assoc()
				       end;
		    false -> new_assoc()
		end,
	    _TrustRoot = case lists:keysearch("openid.trust_root", 1, Query) of
			     {value, {_, Vs}} -> Vs;
			     false -> ReturnTo
			 end,
	    Params = [{"identity",Ident} ,
		      {"return_to",ReturnTo}, {"assoc_handle", AssocHandle} ],
	    {Signed,Sig} = make_signature(Params,Secret),
	    redirect_reply(
	      lists:map(fun({K,V}) -> {"openid_" ++ K, V} end, Params)
	      ++ [{"openid_mode","id_res"},
		  {"openid_signed", Signed},
		  {"openid_sig", Sig}],
	      ReturnTo);
	_ -> redirect_reply([{"openid_mode","error"},
			     {"openid_error","WrongIdentity"}],
			    ReturnTo)
    end.

check_authentication(Query, _Profile) ->
    case lists:keysearch("openid.assoc_handle", 1, Query) of
	{value, {_, AssocHandle} } ->
	    case lists:keysearch("openid.sig", 1, Query) of
		{value, {_, Sig} } ->
		    case lists:keysearch("openid.signed", 1, Query) of
			{value, {_, Signed} } ->
			    direct_reply([{"openid.mode","id_res"},
					  {"is_valid",
					   check_authentication2(AssocHandle, Sig,
								 Signed, Query)}]);
			false -> error_reply("missing sig")
		    end;
		false -> error_reply("missing sig")
	    end;
	false -> error_reply("missing handle")
    end.

%% Helper for check_authentication
%% return "true" if the authentication is valid, "false" otherwhise
check_authentication2(AssocHandle, Sig, Signed, Query) ->
    case secret(AssocHandle) of
	{ok, Secret} ->
	    case catch make_signature(retrieve_params(Signed,Query),Secret) of
		{Signed,Sig} -> "true";
		_ -> "false"
	    end;
	_ -> "false"
    end.

%% Fields is a list of fields which should be in the query as openid.Field
%% return the list of argument [{Key,Value}] as they appears in the query
retrieve_params(Fields,Query) ->
    FList = re:split(Fields, ",", [{return, list}]),
    retrieve_params_recurse(FList,Query).
retrieve_params_recurse([],_) -> [];
retrieve_params_recurse([Key | Tail ], Query) ->
    {value, {_, Value} } = lists:keysearch("openid." ++ Key, 1, Query),
    [ {Key, Value} | retrieve_params_recurse(Tail,Query) ].

not_implemented(Profile) ->
    make_xhtml([{xmlelement,"h1",[],[{xmlcdata, "NOT IMPLEMENTED"}]}],Profile).

make_xhtml(Els,  #profile{lang=Lang} = Profile) ->
    {200, [html],
     {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"},
			   {"xml:lang", Profile#profile.lang},
			   {"lang", Profile#profile.lang}],
      [{xmlelement, "head", [],
	[?XCT("title", "ejabberd OpenId Provider"),
	 {xmlelement, "meta", [{"http-equiv", "Content-Type"},
			       {"content", "text/html; charset=utf-8"}], []},
	 {xmlelement, "link", [{"rel", "openid.server"},
			       {"href", Profile#profile.server}], []},
	 {xmlelement, "link", [{"rel", "openid.delegate"},
			       {"href", Profile#profile.identity}], []}]},
       ?XE("body", Els) ]}}.

error_400(Message) ->
    {400, [], ejabberd_web:make_xhtml([{xmlelement,"h1",[],
					[{xmlcdata, "400 Bad Request"}]},
				       {xmlelement,"p",[],[{xmlcdata, Message}]}])}.

%% Ask the user agent to go to the ReturnTo URL with the specified
%% Params in the query
redirect_reply(Params, ReturnTo) ->
    Delim = case lists:member($?, ReturnTo) of
		true -> $&;
		false -> $?
	    end,
    {303, [{"Location", ReturnTo ++ build_query(Params, Delim)}], []}.

%% Given a list of {Key,Value}, construct the query string, starting
%% with Delim (either '?' or '&')
build_query([ {Key,Value} | Tail ], Delim) ->
    [Delim] ++ Key ++ "=" ++ Value ++ build_query(Tail, $&);
build_query([], _) ->
    [].

%% return the parameters directly in the HTTP reply
direct_reply(Params) ->
    {200, [], build_reply(Params)}.

%% Given a list of {Key,Value}, return the string which should
%% appears in the dirrect reply.
build_reply([ {Key,Value} | Tail ]) ->
    Key ++ ":" ++ Value ++ "\n" ++ build_reply(Tail);
build_reply([]) -> [].

%% Direct reply of an error
error_reply(Message) ->
    {400, [], "error:" ++ Message ++ "\n"}.

%% Given a list of parameters, sign them.
make_signature(Param, Secret) ->
    {field_list(Param),
     jlib:encode_base64(binary_to_list(crypto:sha_mac(Secret, build_reply(Param))))}.

%% Given a list of parameters, return a string containing the list of
%% key separated by comas.
field_list([]) -> [];
field_list([ {Key,_Value} ]) -> Key;
field_list([ {Key,_Value} | Tail ]) ->
    Key ++ "," ++ field_list(Tail).

%% TODO: thoses function need to be implemented for security
%% given an association key, return {ok, Secret} or false
secret(_) -> {ok, "Secret"}.
%% create a new association and return {Key, Secret}
new_assoc() -> {"assoc", "Secret"}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PART 2 : Glue

%% return "verified" if the user agent is authorized,  otherwhise, return an HTTP reply.
verify_id(#request{auth = Auth} = _Request, #profile{ jid = Jid} = _Profile) ->
    %% TODO verify that the Jid in the identity is the same as the Jid in the profile
    Exp = {Jid#jid.luser, Jid#jid.lserver},
    case get_auth(Auth) of
	Exp ->
	    verified;
	C ->
	    ?MYDEBUG("Auth Failed ~p ~n", [C]),
	    {return, {401,
		      [{"WWW-Authenticate", "basic realm=\"ejabberd-mod_openid\""}],
		      ejabberd_web:make_xhtml([{xmlelement, "h1", [],
						[{xmlcdata, "401 Unauthorized"}]}])}}
    end.

%% return the {User,Server} jid if the authentification is correct, or unauthorized
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

