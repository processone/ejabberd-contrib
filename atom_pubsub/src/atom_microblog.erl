%%
% This module enables access to the PEP node "urn:xmpp:microblog" via 
% an Atompub compliant interface.
% 
% 
-module(atom_microblog).
-author('eric@ohmforce.com').
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("pubsub.hrl").
-include("ejabberd_http.hrl").
-include("logger.hrl").
-export([process/2]).
 
process([Domain,User|_]=LocalPath,  #request{auth = Auth} = Request)->
	case get_auth(Auth) of
	%%make sure user belongs to pubsub domain
	{User, Domain} ->
	    out(Request, Request#request.method, LocalPath,User);
	_ ->
	    out(Request, Request#request.method, LocalPath,undefined)
    end;
	

process(_LocalPath, _Request)->
	 error(404).

get_host([Domain,User|_Rest])-> {User, Domain, []}.

get_collection([_Domain,_User, Node|_R])->
	case lists:member(Node, ["mood", "geoloc", "tune"]) of 
		true -> "http://jabber.org/protocol/"++Node;
		false -> Node
	end;
get_collection(_)->error.

get_member([_Domain,_User, _Node, Member]=Uri)->
	[get_host(Uri), get_collection(Uri), Member].
	
base_uri(#request{host=Host, port=Port}, Domain, User)->
	"http://"++Host++":"++i2l(Port)++"/pep/"++Domain++"/"++ User.

collection_uri(R, Domain, User, Node)->
	 Clean=lists:last(string:tokens(Node, "/")),
	 base_uri(R, Domain, User)++"/"++Clean.

entry_uri(R, Domain, User, Node, Id)->
	collection_uri(R, Domain, User, Node)++"/"++Id.

generate_etag(#pubsub_item{modification={_JID, {_, D2, D3}}})->integer_to_list(D3+D2).

out(_Args, 'POST', [_,_, _], undefined) ->error(401);
out(_Args, 'PUT', [_,_, _], undefined) ->error(401);	
out(_Args, 'DELETE', [_,_, _], undefined) ->error(401);


%% Service document
out(Args, 'GET', [Domain, UserNode]=Uri, _User) ->
	%%Collections = mnesia:dirty_match_object(#pubsub_node{nodeid={get_host(Uri), '_'},_ = '_'}),
	case mod_pubsub:tree_action(get_host(Uri), get_nodes, [get_host(Uri)]) of
		[] -> error(404);
		Collections ->
			?DEBUG("PEP nodes : ~p~n",[Collections]),
			{200, [{"Content-Type", "application/atomsvc+xml"}], "<?xml version=\"1.0\" encoding=\"utf-8\"?>" 
				++	xml:element_to_string(service(Args,Domain, UserNode, Collections))}
	end;

%% Collection

out(Args, 'GET', [Domain, User, Node]=Uri, _User) -> 
    case mod_pubsub:tree_action(get_host(Uri), get_node, [get_host(Uri),get_collection(Uri)]) of
	{error, _} -> error(404);
	_ ->
		Items = lists:sort(fun(X,Y)->
				{_,DateX} = X#pubsub_item.modification,
				{_,DateY} = Y#pubsub_item.modification,
				DateX > DateY
			end, mod_pubsub:get_items(
					get_host(Uri),
					get_collection(Uri), "")),
		case Items of
			[] -> ?DEBUG("Items : ~p ~n", [collection(get_collection(Uri), 
				collection_uri(Args,Domain,User,Node), calendar:now_to_universal_time(erlang:now()), User, "", [])]),
				{200, [{"Content-Type", "application/atom+xml"}],
					collection(get_collection(Uri), 
						collection_uri(Args,Domain,User,Node), calendar:now_to_universal_time(erlang:now()), User, "", [])};
			_ ->
				#pubsub_item{modification = {_JID,LastDate}} = LastItem = hd(Items),
				Etag =generate_etag(LastItem),
				IfNoneMatch=proplists:get_value('If-None-Match', Args#request.headers),
				if IfNoneMatch==Etag
					-> 
						success(304);
					true ->
						XMLEntries= [item_to_entry(Args,Node,Entry)||Entry <-  Items], 
						{200, [{"Content-Type", "application/atom+xml"},{"Etag", Etag}], 
						"<?xml version=\"1.0\" encoding=\"utf-8\"?>" 
						++	xml:element_to_string(
						collection(get_collection(Uri), collection_uri(Args,Domain,User,Node),
							calendar:now_to_universal_time(LastDate), User, "", XMLEntries))}
			end
		end
	end;
%% Add new collection
out(_Args, 'POST', [_Domain, _User], _User)-> error(403);

out(Args, 'POST', [Domain,User, Node]=Uri, User) -> 
	%%FIXME Slug
	Slug = case lists:keysearch("Slug",3,Args#request.headers) of 
		false -> uniqid(false) ; 
		{value, {_,_,_,_,Value}} -> Value 
	end,
	Payload = xml_stream:parse_element(Args#request.data),
	case mod_pubsub:publish_item(get_host(Uri),
								 Domain,
								 get_collection(Uri),
								 jlib:make_jid(User,Domain, ""), 
								 Slug, 
								 [Payload]) of
		{result, []} ->
				?DEBUG("Publishing to ~p~n",[entry_uri(Args, Domain,User, Node,Slug)]),
			{201, [{"location", entry_uri(Args, Domain,User,Node,Slug)}], Payload};
		{error, Error} ->
			error(400, Error)
		end;
out(_Args, 'POST', [_, _, _], _) ->
	{status, 403};
			
%% Atom doc
out(Args, 'GET', [_Domain,_U, Node, _Member]=URI, _User) -> 
	Failure = fun(_Error)->error(404)end,
	Success = fun(Item)->
		Etag =generate_etag(Item),
		IfNoneMatch=proplists:get_value('If-None-Match', Args#request.headers),
		if IfNoneMatch==Etag
			-> 
				success(304);
			true ->
		{200, [{"Content-Type",  "application/atom+xml"},{"Etag", Etag}], "<?xml version=\"1.0\" encoding=\"utf-8\"?>" 
				++ xml:element_to_string(item_to_entry(Args, Node, Item))}
		end
	end,
	get_item(URI, Failure, Success);
		

%% Update doc
out(Args, 'PUT', [Domain,User, _Node, Member]=Uri, User) -> 
	Payload = xml_stream:parse_element(Args#request.data),
	Failure = fun(_Error)->error(404)end,
	Success = fun(Item)->
		Etag =generate_etag(Item),
		IfMatch=proplists:get_value('If-Match', Args#request.headers),
		if IfMatch==Etag
			-> 
			case mod_pubsub:publish_item(get_host(Uri),
										 Domain,
										 get_collection(Uri),
										 jlib:make_jid(User,Domain, ""), 
										 Member, 
										 [Payload]) of
				{result, _Result} -> 
					{200, [{"Content-Type",  "application/atom+xml"}],""};
				{error, {xmlelement, "error", [{"code",Code},_],_}} ->
					error(Code);
				{error, _Error} ->
					error(500)
				end;
			true ->
				error(412) %% ressource has been modified since last get for this client.
		end
	end,
	get_item(Uri, Failure, Success);
	
%%
out(_Args, 'PUT',_Url, _User) ->
	error(401);

out(_Args, 'DELETE', [Domain,User, _Node, _Member]=Uri, User) ->
	case mod_pubsub:delete_item(get_host(Uri), 
								get_collection(Uri),
								jlib:make_jid(User,Domain, ""),
								get_member(Uri)) of
		{result, _Result} -> 
			success(200);
		{error, {xmlelement, "error", [{"code",Code},_],_}} ->
			error(Code);
		{error, _Code1} ->
			error(500)
		end;

out(_Args, 'DELETE',_Url, _User) ->
		error(401);	
		
out(_, _, _, _) ->
	error(403).

get_item(Uri, Failure, Success)->
	case catch mod_pubsub:node_action(get_host(Uri), 
									get_collection(Uri),
									get_item, 
									get_member(Uri)) of
		{error, Reason} ->
			Failure(Reason);
		{result, Item} ->
			Success(Item)
		end.
		

	

item_to_entry(Args,Node,#pubsub_item{itemid={Id,_}, payload=Entry}=Item)->
	[R | _]=xml:remove_cdata(Entry),
	item_to_entry(Args, Node, Id, R, Item).

item_to_entry(Args,Node,  Id,{xmlelement, "entry", Attrs, SubEl}, 
		#pubsub_item{modification={_, Secs}, itemid={Id, {{User, Domain, []},_}}}) ->	
	Date = calendar:now_to_local_time(Secs),
	SubEl2=[{xmlelement, "app:edited", [], [{xmlcdata, w3cdtf(Date)}]},
			{xmlelement, "link",[{"rel", "edit"}, 
			{"href", entry_uri(Args,Domain,User, Node, Id)}],[] }, 
			{xmlelement, "id", [],[{xmlcdata, Id}]}
			| SubEl],
	{xmlelement, "entry", [{"xmlns:app","http://www.w3.org/2007/app"}|Attrs], SubEl2};
	
%% Don't do anything except adding xmlns
item_to_entry(_Args,Node,  _Id, {xmlelement, Name, Attrs, Subels}=Element, _Item)->
	case proplists:is_defined("xmlns",Attrs) of
		true -> Element;
		false -> {xmlelement, Name, [{"xmlns", Node}|Attrs], Subels}
	end.
	
	

collection(Title, Link, Updated, Author, _Id, Entries)->
	{xmlelement, "feed", [{"xmlns", "http://www.w3.org/2005/Atom"}, 
						 {"xmlns:app", "http://www.w3.org/2007/app"}], [
		{xmlelement, "title", [],[{xmlcdata, Title}]},
		{xmlelement, "updated", [],[{xmlcdata, w3cdtf(Updated)}]},
		{xmlelement, "link", [{"href", Link}], []},
		{xmlelement, "author", [], [
			{xmlelement, "name", [], [{xmlcdata,Author}]}
		]},
		{xmlelement, "title", [],[{xmlcdata, Title}]} | 
		Entries
	]}.

service(Args, Domain, User, Collections)->
	{xmlelement, "service", [{"xmlns", "http://www.w3.org/2007/app"},
							{"xmlns:atom", "http://www.w3.org/2005/Atom"},
							{"xmlns:app", "http://www.w3.org/2007/app"}],[
		{xmlelement, "workspace", [],[
			{xmlelement, "atom:title", [],[{xmlcdata,"Feed for "++User++"@"++Domain}]} | 
			lists:map(fun(#pubsub_node{nodeid={_Server, Id}})->
				{xmlelement, "collection", [{"href", collection_uri(Args,Domain,User, Id)}], [
					{xmlelement, "atom:title", [], [{xmlcdata, Id}]}
				]}
				end, Collections)
		]}
	]}.

%%% lifted from ejabberd_web_admin
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



%% simple output functions
error(404)->
	{404, [], "Not Found"};
error(403)->
	{403, [], "Forbidden"};
error(500)->
	{500, [], "Internal server error"};
error(401)->
	{401, [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],"Unauthorized"};
error(Code)->
	{Code, [], ""}.
success(200)->
	{200, [], ""};
success(Code)->
	{Code, [], ""}.
error(Code, Error) when is_list(Error) -> {Code, [], Error};
error(Code, {xmlelement, "error",_,_}=Error) -> {Code, [], xml:element_to_string(Error)};
error(Code, _Error) -> {Code, [], "Bad request"}.

	
% Code below is taken (with some modifications) from the yaws webserver, which
% is distributed under the folowing license:
%
% This software (the yaws webserver) is free software.
% Parts of this software is Copyright (c) Claes Wikstrom <klacke@hyber.org>
% Any use or misuse of the source code is hereby freely allowed.
%
% 1. Redistributions of source code must retain the above copyright
%    notice as well as this list of conditions.
%
% 2. Redistributions in binary form must reproduce the above copyright
%    notice as well as this list of conditions.
%%% Create W3CDTF (http://www.w3.org/TR/NOTE-datetime) formatted date
%%% w3cdtf(GregSecs) -> "YYYY-MM-DDThh:mm:ssTZD"
%%%
uniqid(false)->
	{T1, T2, T3} = now(),
    lists:flatten(io_lib:fwrite("~.16B~.16B~.16B", [T1, T2, T3]));
uniqid(Slug) ->
	Slut = string:to_lower(Slug),
	S = string:substr(Slut, 1, 9),
    {_T1, T2, T3} = now(),
    lists:flatten(io_lib:fwrite("~s-~.16B~.16B", [S, T2, T3])).

w3cdtf(Date) -> %1   Date = calendar:gregorian_seconds_to_datetime(GregSecs),
    {{Y, Mo, D},{H, Mi, S}} = Date,
    [UDate|_] = calendar:local_time_to_universal_time_dst(Date),
    {DiffD,{DiffH,DiffMi,_}}=calendar:time_difference(UDate,Date),
    w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi). 

%%%  w3cdtf's helper function
w3cdtf_diff(Y, Mo, D, H, Mi, S, _DiffD, DiffH, DiffMi) when DiffH < 12,  DiffH /= 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "+" ++ add_zero(DiffH) ++ ":"  ++ add_zero(DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi) when DiffH > 12,  DiffD == 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "+" ++ add_zero(DiffH) ++ ":"  ++
        add_zero(DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi) when DiffH > 12,  DiffD /= 0, DiffMi /= 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "-" ++ add_zero(23-DiffH) ++
        ":" ++ add_zero(60-DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi) when DiffH > 12,  DiffD /= 0, DiffMi == 0 ->
   i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "-" ++ add_zero(24-DiffH) ++
        ":" ++ add_zero(DiffMi); 

w3cdtf_diff(Y, Mo, D, H, Mi, S, _DiffD, DiffH, _DiffMi) when DiffH == 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "Z".

add_zero(I) when is_integer(I) -> add_zero(i2l(I));
add_zero([A])               -> [$0,A];
add_zero(L) when is_list(L)    -> L. 

i2l(I) when is_integer(I) -> integer_to_list(I);
i2l(L) when is_list(L)    -> L.


