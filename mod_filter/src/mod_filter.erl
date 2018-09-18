%%%----------------------------------------------------------------------
%%% File    : mod_filter.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : flexible filtering by server policy
%%% Created : 21 Sep 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%----------------------------------------------------------------------

-module(mod_filter).
-author('henoch@dtek.chalmers.se').

-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, filter_packet/1]).

-include("logger.hrl").
-include("xmpp.hrl").

start(_Host, _Opts) ->
    ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 100).


stop(_Host) ->
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, filter_packet, 100).

%% Return drop to drop the packet, or the original input to let it through.
%% From and To are jid records.
filter_packet(drop) ->
    drop;
filter_packet(Packet) ->
    From = xmpp:get_from(Packet),
    To = xmpp:get_to(Packet),
    %% It probably doesn't make any sense to block packets to oneself.
    R = if From#jid.luser == To#jid.luser,
	   From#jid.lserver == To#jid.lserver ->
		Packet;
	   true ->
		check_stanza(Packet)
	end,
    ?DEBUG("filtering packet...~nFrom: ~p~nTo: ~p~nPacket: ~p~nResult: ~p",
	   [From, To, Packet, R]),
    case R of
	{drop, _} -> drop;
	{drop, _, _} -> drop;
	_ -> R
    end.

check_stanza(Packet) ->
    AccessRule = case element(1, Packet) of
		     presence ->
			 mod_filter_presence;
		     message ->
			 mod_filter_message;
		     iq ->
			 mod_filter_iq
		 end,
    check_stanza_type(AccessRule, Packet).

check_stanza_type(AccessRule, Packet) ->
    FromAccess = acl:match_rule(global, AccessRule, xmpp:get_from(Packet)),
    case FromAccess of
	allow ->
	    check_access(Packet);
	deny ->
	    {drop, AccessRule, sender};
	ToAccessRule ->
	    ToAccess = acl:match_rule(global, ToAccessRule, xmpp:get_to(Packet)),
	    case ToAccess of
		allow ->
		    check_access(Packet);
		deny ->
		    {drop, AccessRule, receiver}
	    end
    end.

check_access(Packet) ->
    %% Beginning of a complicated ACL matching procedure.
    %% The access option given to the module applies to senders.

    %% XXX: there are no "global" module options, and we don't know
    %% anymore what "host" we are on.  Thus hardcoding access rule.
    %%AccessRule = gen_mod:get_module_opt(global, ?MODULE, access, all),
    AccessRule = mod_filter,
    FromAccess = acl:match_rule(global, AccessRule, xmpp:get_from(Packet)),
    %% If the rule results in 'allow' or 'deny', treat that as the
    %% result.  Else it is a rule to be applied to the receiver.
    case FromAccess of
	allow ->
	    Packet;
	deny ->
	    {drop, sender};
	ToAccessRule ->
	    ToAccess = acl:match_rule(global, ToAccessRule, xmpp:get_to(Packet)),
	    case ToAccess of
		allow ->
		    Packet;
		deny ->
		    {drop, receiver}
	    end
    end.


depends(_Host, _Opts) ->
    [].

mod_options(_) -> [].
