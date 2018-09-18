%%%----------------------------------------------------------------------
%%% File    : mod_filter.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : flexible filtering by server policy
%%% Created : 21 Sep 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
%%% Updated : 14 Jan 2016 by John Brodie <john@brodie.me>
%%%----------------------------------------------------------------------

-module(mod_filter).
-author('henoch@dtek.chalmers.se').
%% -vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 filter_packet/1, mod_opt_type/1]).

-include("logger.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").

start(_Host, _Opts) ->
    ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 100).


stop(_Host) ->
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, filter_packet, 100).

%% Return drop to drop the packet, or the original input to let it through.
%% From and To are jid records.
filter_packet(drop) ->
    drop;
filter_packet({From, To, Packet} = Input) ->
    %% It probably doesn't make any sense to block packets to oneself.
    R = if From#jid.luser == To#jid.luser,
	   From#jid.lserver == To#jid.lserver ->
		Input;
	   true ->
		check_stanza(Input)
	end,
    ?DEBUG("filtering packet...~nFrom: ~p~nTo: ~p~nPacket: ~p~nResult: ~p",
	   [From, To, Packet, R]),
    case R of
	{drop, _} -> drop;
	{drop, _, _} -> drop;
	_ -> R
    end.

check_stanza({_From, _To, #xmlel{name = StanzaType}} = Input) ->
    AccessRule = case StanzaType of
		     <<"presence">> ->
			 mod_filter_presence;
		     <<"message">> ->
			 mod_filter_message;
		     <<"iq">> ->
			 mod_filter_iq
		 end,
    check_stanza_type(AccessRule, Input).

check_stanza_type(AccessRule, {From, To, _Packet} = Input) ->
    FromAccess = acl:match_rule(global, AccessRule, From),
    case FromAccess of
	allow ->
	    check_access(Input);
	deny ->
	    {drop, AccessRule, sender};
	ToAccessRule ->
	    ToAccess = acl:match_rule(global, ToAccessRule, To),
	    case ToAccess of
		allow ->
		    check_access(Input);
		deny ->
		    {drop, AccessRule, receiver}
	    end
    end.

check_access({From, To, _Packet} = Input) ->
    %% Beginning of a complicated ACL matching procedure.
    %% The access option given to the module applies to senders.

    %% XXX: there are no "global" module options, and we don't know
    %% anymore what "host" we are on.  Thus hardcoding access rule.
    %%AccessRule = gen_mod:get_module_opt(global, ?MODULE, access, all),
    AccessRule = mod_filter,
    FromAccess = acl:match_rule(global, AccessRule, From),
    %% If the rule results in 'allow' or 'deny', treat that as the
    %% result.  Else it is a rule to be applied to the receiver.
    case FromAccess of
	allow ->
	    Input;
	deny ->
	    {drop, sender};
	ToAccessRule ->
	    ToAccess = acl:match_rule(global, ToAccessRule, To),
	    case ToAccess of
		allow ->
		    Input;
		deny ->
		    {drop, receiver}
	    end
    end.

mod_opt_type(access) ->
    fun (A) when is_atom(A) -> A end;

mod_opt_type(_) ->
    [access].
