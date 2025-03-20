-module(mod_spam_filter_rtbl).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").

-define(RTBL_DOMAINS_NODE, <<"spam_source_domains">>).
-define(SERVICE_JID_PREFIX, "rtbl-").

-export([parse_blocked_domains/1,
	 parse_pubsub_event/1,
	 pubsub_event_handler/1,
	 request_blocked_domains/2,
	 subscribe/2,
	 unsubscribe/2]).

subscribe(RTBLHost, From) ->
    FromJID = service_jid(From),
    SubIQ = #iq{type = set, to = jid:make(RTBLHost), from = FromJID,
		sub_els = [
			   #pubsub{subscribe = #ps_subscribe{jid = FromJID, node = ?RTBL_DOMAINS_NODE}}]},
    ?DEBUG("Sending subscription request:~n~p", [xmpp:encode(SubIQ)]),
    ejabberd_router:route_iq(SubIQ, subscribe_result, self()).

unsubscribe(undefined, _From) ->
    ok;
unsubscribe(RTBLHost, From) ->
    FromJID = jid:make(From),
    SubIQ = #iq{type = set, to = jid:make(RTBLHost), from = FromJID,
		sub_els = [
			   #pubsub{unsubscribe = #ps_unsubscribe{jid = FromJID, node = ?RTBL_DOMAINS_NODE}}]},
    ejabberd_router:route_iq(SubIQ, unsubscribe_result, self()).

-spec request_blocked_domains(binary(), binary()) -> ok.
request_blocked_domains(undefined, _From) ->
    ok;
request_blocked_domains(RTBLHost, From) ->
    IQ = #iq{type = get, from = jid:make(From),
	     to = jid:make(RTBLHost),
	     sub_els = [
			#pubsub{items = #ps_items{node = ?RTBL_DOMAINS_NODE}}]},
    ?DEBUG("Requesting RTBL blocked domains from ~s:~n~p", [RTBLHost, xmpp:encode(IQ)]),
    ejabberd_router:route_iq(IQ, blocked_domains, self()).

-spec parse_blocked_domains(stanza()) -> #{binary() => any()} | undefined.
parse_blocked_domains(#iq{from = From, type = result} = IQ) ->
    ?DEBUG("parsing fetched items from ~p", [From]),
    case xmpp:get_subtag(IQ, #pubsub{}) of
	#pubsub{items = #ps_items{node = ?RTBL_DOMAINS_NODE, items = Items}} ->
	    ?DEBUG("Got items:~n~p", [Items]),
	    parse_items(Items);
	_ ->
	    undefined
    end.

-spec parse_pubsub_event(stanza()) -> #{binary() => any()}.
parse_pubsub_event(Msg) ->
    case xmpp:get_subtag(Msg, #ps_event{}) of
	#ps_event{items = #ps_items{node = ?RTBL_DOMAINS_NODE, retract = ID}} when ID /= undefined ->
	    ?DEBUG("Got item to retract:~n~p", [ID]),
	    #{ID => false};
	#ps_event{items = #ps_items{node = ?RTBL_DOMAINS_NODE, items = Items}} ->
	    ?DEBUG("Got items:~n~p", [Items]),
	    parse_items(Items);
	Other ->
	    ?WARNING_MSG("Couldn't extract items: ~p", [Other]),
	    #{}
    end.

-spec parse_items([ps_item()]) -> #{binary() => any()}.
parse_items(Items) ->
    lists:foldl(
      fun(#ps_item{id = ID}, Acc) ->
	      %% TODO extract meta/extra instructions
	      maps:put(ID, true, Acc)
      end, #{}, Items).

-spec service_jid(binary()) -> jid().
service_jid(Host) ->
    jid:make(<<>>, Host, <<?SERVICE_JID_PREFIX, (ejabberd_cluster:node_id())/binary>>).

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------

-spec pubsub_event_handler(stanza()) -> drop | stanza().
pubsub_event_handler(#message{from = FromJid,
			      to = #jid{lserver = LServer,
					lresource = <<?SERVICE_JID_PREFIX, _/binary>>}} = Msg) ->

    ?DEBUG("Got RTBL message to:~n~p", [Msg]),
    From = jid:encode(FromJid),
    case gen_mod:get_module_opt(LServer, mod_spam_filter, rtbl_host) of
	From ->
	    ParsedItems = parse_pubsub_event(Msg),
	    Proc = gen_mod:get_module_proc(LServer, mod_spam_filter),
	    gen_server:cast(Proc, {update_blocked_domains, ParsedItems}),
	    %% FIXME what's the difference between `{drop, ...}` and `{stop, {drop, ...}}`?
	    drop;
	_Other ->
	    ?INFO_MSG("Got unexpected message from ~s to rtbl resource:~n~p", [From, Msg]),
	    Msg
    end;
pubsub_event_handler(Acc) ->
    ?DEBUG("unexpected something on pubsub_event_handler: ~p", [Acc]),
    Acc.
