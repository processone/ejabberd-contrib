%%%----------------------------------------------------------------------
%%% File    : mod_profile.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Store profile stanzas in Mnesia table (XEP-0154 0.5 IQ semantics)
%%% Created : 22 Oct 2006 by Magnus Henoch <henoch@dtek.chalmers.se>
%%% Id      : $Id: mod_profile.erl 1039 2009-11-24 22:47:40Z badlop $
%%%----------------------------------------------------------------------

%%%% Documentation

%%% Protocol implementation (as of XEP-0154 0.6)
%%%
%%% 4.1. Publishing a Full Profile
%%% + Stanza is processed, and a response is returned
%%% - But the stanza is stored in a Mnesia table, not in PubSub
%%%
%%% 4.2. Updating One or More Profile Fields
%%% - Not possible because PubSub is not used for storage
%%%
%%% 5.1. Discovering Support
%%% + Fully implemented
%%%
%%% 5.2. Requesting Full Profile
%%% + Stanza is processed, and a response is returned
%%% - The information is read from Mnesia table, not from PubSub
%%%
%%% 5.3. Receiving Profile Updates
%%% - Does not work because storage is done on Mnesia, not PubSub
%%%
%%% 7. Security Considerations
%%% - Implement Access option to restrict who is allowed to read/write profiles
%%% - any pubsub node it may create for profile data has an access model of "presence" or "roster"
%%%
%%%
%%% The server should include code to forward information of IQ queries into PubSub,
%%% so all the information provided with any semantic mentioned in XEP-0154
%%% must be stored in PubSub.
%%% stpeter: I think the idea was (B), but it's not well-defined
%%%
%%%
%%% Custom IQ semantics for XEP-0154:
%%%
%%% 5.4. Requesting One or More Profile Fields
%%%
%%% A user can query some specific fields from his profile:
%%%
%%% <iq type='get' to='user@localhost'>
%%%   <profile xmlns='urn:xmpp:tmp:profile'>
%%%     <x xmlns='jabber:x:data' type='submit'>
%%%       <field var='FORM_TYPE' type='hidden'><value>urn:xmpp:tmp:profile</value></field>
%%%       <field var='nickname'/>
%%%       <field var='country'/>
%%%       <field var='postal_code'/>
%%%     </x>
%%%   </profile>
%%% </iq>
%%%
%%% The server will return only the fields that were requested and the user had defined previously:
%%%
%%% <iq from='user@localhost' type='result' to='user@localhost/Home'>
%%%   <profile xmlns='urn:xmpp:tmp:profile'>
%%%     <x xmlns='jabber:x:data' type='result'>
%%%       <field var='FORM_TYPE' type='hidden'><value>urn:xmpp:tmp:profile</value></field>
%%%       <field var='nickname'><value>Drummy</value></field>
%%%       <field var='country'><value>DK</value></field>
%%%     </x>
%%%   </profile>
%%% </iq>
%%%
%%% 4.3. Updating One or More Profile Fields using IQ Semantics
%%%
%%% - if a field is set empty value, delete field in the stored profile
%%%

%%%=======================
%%%% Headers

-module(mod_profile).

-author('henoch@dtek.chalmers.se').

-behaviour(gen_mod).

-export([start/2, stop/1, process_sm_iq/3,
	 get_sm_features/5, remove_user/2]).

-include("ejabberd.hrl").

-include("jlib.hrl").

-include("logger.hrl").

-record(profile, {us, fields}).

-define(NS_PROFILE, <<"urn:xmpp:tmp:profile">>).

%%%=======================
%%%% gen_mod

start(Host, Opts) ->
    mnesia:create_table(profile,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, profile)}]),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       get_sm_features, 50),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_PROFILE, ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  get_sm_features, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_PROFILE).

%%%=======================
%%%% Hooks

get_sm_features({error, _} = Acc, _From, _To, _Node,
		_Lang) ->
    Acc;
get_sm_features(Acc, _From, _To, Node, _Lang) ->
    case Node of
      [] ->
	  case Acc of
	    {result, Features} ->
		{result, [?NS_PROFILE | Features]};
	    empty -> {result, [?NS_PROFILE]}
	  end;
      _ -> Acc
    end.

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun () -> mnesia:delete({profile, US}) end,
    mnesia:transaction(F).

%%%=======================
%%%% IQ handler

process_sm_iq(From, To,
	      #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  #jid{luser = LUser, lserver = LServer} = From,
	  process_sm_iq_set(LUser, LServer, SubEl, IQ);
      get ->
	  #jid{luser = LUser, lserver = LServer} = To,
	  process_sm_iq_get(LUser, LServer, SubEl, IQ)
    end.

process_sm_iq_set(LUser, LServer, SubEl, IQ) ->
    case lists:member(LServer, ?MYHOSTS) of
      true ->
	  #xmlel{children = SubSubEls} = SubEl,
	  ElsList = [El
		     || #xmlel{name = Name} = El
			    <- xml:remove_cdata(SubSubEls),
			Name == <<"x">>],
	  case ElsList of
	    [XData] ->
		case set_profile(LUser, LServer, XData) of
		  ok -> IQ#iq{type = result, sub_el = []};
		  {error, Error} ->
		      IQ#iq{type = error, sub_el = [SubEl, Error]}
		end;
	    _ ->
		IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
	  end;
      false ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.

process_sm_iq_get(LUser, LServer, SubEl, IQ) ->
    ReqFields = get_requested_fields(SubEl),
    case get_profile(LUser, LServer, ReqFields) of
      {ok, Fields} ->
	  XEl = #xmlel{name = <<"x">>,
		       attrs =
			   [{<<"xmlns">>, <<"jabber:x:data">>},
			    {<<"type">>, <<"result">>}],
		       children = Fields},
	  ProfileEl = #xmlel{name = <<"profile">>,
			     attrs = [{<<"xmlns">>, ?NS_PROFILE}],
			     children = [XEl]},
	  IQ#iq{type = result, sub_el = [ProfileEl]};
      {error, user_not_found} ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
      {error, OtherError} ->
	  ?ERROR_MSG("Problem found when getting profile of "
		     "~p@~p:~n~p",
		     [LUser, LServer, OtherError]),
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
    end.

%%%=======================
%%%% Set profile

set_profile(LUser, LServer,
	    #xmlel{name = <<"x">>, children = Els}) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:write(#profile{us = US, fields = Els})
	end,
    case mnesia:transaction(F) of
      {atomic, _} -> ok;
      _ -> {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

%%%=======================
%%%% Get profile

get_profile(LUser, LServer, []) ->
    US = {LUser, LServer},
    F = fun () -> mnesia:read({profile, US}) end,
    case mnesia:transaction(F) of
      {atomic, [#profile{fields = Fields}]} -> {ok, Fields};
      {atomic, []} -> {error, user_not_found};
      OtherResult -> {error, OtherResult}
    end;
get_profile(LUser, LServer, ReqFields) ->
    case get_profile(LUser, LServer, []) of
      {ok, Fields} ->
	  filter_profile_fields(Fields, ReqFields);
      Other -> Other
    end.

filter_profile_fields(Fields, ReqFields) ->
    filter_profile_fields(Fields, ReqFields, []).

filter_profile_fields(StoredFields,
		      [#xmlel{name = <<"field">>, attrs = Attrs,
			      children = []}
		       | ReqFields],
		      ResFields) ->
    case lists:keysearch(Attrs, 3, StoredFields) of
      {value, StoredField} ->
	  filter_profile_fields(StoredFields, ReqFields,
				[StoredField | ResFields]);
      false ->
	  filter_profile_fields(StoredFields, ReqFields,
				ResFields)
    end;
filter_profile_fields(StoredFields,
		      [_FieldForm | ReqFields], ResFields) ->
    filter_profile_fields(StoredFields, ReqFields,
			  ResFields);
filter_profile_fields(_StoredFields, [], ResFields) ->
    FieldFormType = #xmlel{name = <<"field">>,
			   attrs =
			       [{<<"var">>, <<"FORM_TYPE">>},
				{<<"type">>, <<"hidden">>}],
			   children =
			       [#xmlel{name = <<"value">>, attrs = [],
				       children =
					   [{xmlcdata,
					     <<"urn:xmpp:tmp:profile">>}]}]},
    {ok, [FieldFormType | lists:reverse(ResFields)]}.

%% TODO: la respuesta a una iq query de fields especificos ha de incluir

%%%=======================
%%%% Mnesia storage

%%%=======================
%%%% PubSub storage

%%%=======================
%%%% XML processing

%% Copied from exmpp_xml.erl, then customized

get_requested_fields(SubEl) ->
    case xml:get_subtag(SubEl, <<"x">>) of
      false -> [];
      XEl -> get_elements(XEl, <<"field">>)
    end.

get_elements(#xmlel{name = <<"x">>,
		    children = Children},
	     Name) ->
    get_elements2(Children, Name);
get_elements(_, _Name) -> [].

get_elements2([], _Name) -> [];
get_elements2(Children, Name) ->
    lists:filter(filter_by_name(Name), Children).

filter_by_name(Searched_Name) ->
    fun (XML_Element) ->
	    element_matches(XML_Element, Searched_Name)
    end.

element_matches(#xmlel{name = Name}, Name) -> true;
element_matches(_XML_Element, _Name) -> false.

%%%================

%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:

