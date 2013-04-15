%%%-------------------------------------------------------------------
%% @copyright Process One 2008-2009
%% @author Geoff Cant <geoff.cant@process-one.net>
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc DNS Lookup and utility functions.
%% 
%% Provides a programmer-friendly API for a number of undocumented OTP
%% dns lookup, resolution, caching and configuration functions.
%% 
%% Also provides utility functions for performing lookups while
%% bypassing local resolver caching, finding closest parent zones,
%% parsing `resolv.conf' files, simplifying #dns_rec{} answers and more.
%% @end
%%%-------------------------------------------------------------------
-module(dns).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([lookup/2
         ,lookup/3
         ,lookup/4
         ,lookup/5
         ,lookup/7
         ,lookup_cache/3
         ,cache_lookup/1
         ,simplify/1]).

-export([find_soa/1
         ,nameservers/1
         ,nameservers/2
         ,nameserver_address/1
         ,direct_lookup/3
         ,info/2
         ,to_proplist/1
         ,parse_resolv/1
         ,domain_exists/1
         ,expand_options/1
         ,resolvers/0
        ]).

-include_lib("kernel/src/inet_dns.hrl").
-include_lib("kernel/include/inet.hrl").

%% @type query_class() = in.
%% An atom with the name of a DNS query class. `in' is the only class
%% used (and probably tested) in the OTP lookup code.

%% @type query_type() = a | cname | soa | mx | ns | srv | any.
%% An atom with the name of a DNS query type. This list is not
%% exhaustive.

%% @type simple_rr() = {Name::string(), query_type(), RRdata::term(), [simple_rr_info()]}.
%% A simplified version of #dns_rr{} resource records.

%% @type simple_rr_info() = {ttl, TimeToLive::integer()} | {class, query_class()}.

%% @type ip_address() = {integer(),integer(),integer(),integer()}.

%% @type address() = ip_address() | {ip_address(), port_no()} |
%% string() | #dns_rr{}.

%% @type port_no() = integer().

%% @type resolver() = {ip_address(), port_no()}.

%% @type query_options() = [query_option()].
%% A proplist of configuration options for lookup behaviour.

%% @type query_option() = {read_cache, bool()} |
%%                        {write_cache, bool()} |
%%                        {timeout, TimeOut::integer()} |
%%                        {servers, [address()]} |
%%                        {class, query_class()} |
%%                        defaults | read_cached.
%% <ul>
%%  <li>`read_cache' - if `true' (default: `false'), return results from the `inet_db' cache
%%      before making a query over the network.</li>
%%  <li>`write_cache' - if `true' (default: `false'), write successful query result RRs to
%%      the `inet_db' cache</li>
%%  <li>`timeout' - number of milliseconds (default: 5000) to wait for a response from a
%%      nameserver.</li>
%%  <li>`servers' - a list of nameservers to query for the RRs. Defaults
%%      to `inet_db:res_option(nameserver)' which is usually the nameservers
%%      specified in '/etc/resolv.conf'</li>
%%  <li>`class' - record class to query for (default: `in').</li>
%%  <li>`defaults' - shorthand for
%%  <code>
%%     [{servers, dns:resolvers()},
%%      {read_cache, false}, {write_cache, false},
%%      {timeout, timer:seconds(5)}, {class, in}]
%%  </code>
%%  </li>
%%  <li>`read_cached' - shorthand for
%%  <code>
%%     [{servers, dns:resolvers()},
%%      {read_cache, true}, {write_cache, false},
%%      {timeout, timer:seconds(5)}, {class, in}]
%%  </code>
%%  </li>
%% </ul>

%%====================================================================
%% RR lookup API
%%====================================================================

%% @spec lookup(Name::string(), query_type()) -> #dns_res{}
%% @doc Query for a DNS record of Type for Name. Uses reasonable default
%% options for class (`in') timeouts (5s), caching (no caching) and
%% nameservers (`inet_db' defaults).
%% @end
%% @equiv lookup(Name, Type, [defaults])
lookup(Name, Type)
  when is_list(Name), is_atom(Type) ->
    lookup(Name, Type, [defaults]).

%% @spec lookup(Name::string(), query_type(), query_options()) -> #dns_res{}
%% @doc Query for a DNS record of Type for Name. Takes a variety of
%% query options.
%% @end
lookup(Name, Type, Options)
  when is_list(Name), is_atom(Type),
       is_list(Options) ->
    lookup2(Name, Type, expand_options(Options)).

%% @private
lookup2(Name, Type, Options) ->
    Timeout = proplists:get_value(timeout, Options, timer:seconds(5)),
    Servers = [nameserver_address(R)
	       || R <- proplists:get_value(servers, Options, resolvers())],
    Class = proplists:get_value(class, Options, in),
    ReadCache = proplists:get_value(read_cache, Options, false),
    WriteCache = proplists:get_value(write_cache, Options, false),
    lookup(Name, Class, Type, Servers, Timeout, ReadCache, WriteCache).

%% @spec lookup(Name::string(), query_type(),
%%              NameServers, Timeout::integer()) -> #dns_res{}
%% where NameServers = [resolver()]
%% @doc Query the given Nameservers for a DNS record of Type (class `in') for
%% Name. Takes a Timeout in milliseconds. Doesn't read or write the
%% `inet_db' RR cache.
%% @equiv lookup(Name, in, Type, Servers, Timeout, false, false)
lookup(Name, Type, Servers, Timeout)
  when is_list(Name), is_atom(Type),
       is_list(Servers), is_integer(Timeout) ->
    lookup(Name, in, Type, Servers, Timeout).

%% @spec lookup(Name::string(), query_class(), query_type(),
%%              [resolver()], Timeout::integer()) -> #dns_res{}
%% @doc Query the given Nameservers for a DNS record of Class, Type for
%% Name. Takes a Timeout in milliseconds. Doesn't read or write the
%% `inet_db' RR cache.
%% @equiv lookup(Name, Class, Type, Servers, Timeout, false, false)
lookup(Name, Class, Type, Servers, Timeout) 
  when is_list(Name), is_atom(Class), is_atom(Type),
       is_list(Servers), is_integer(Timeout) ->
    lookup(Name, Class, Type, Servers, Timeout, false, false).

%% @spec lookup(Name::string(), class(), query_type(),
%%              Servers::[address()], Timeout::integer(),
%%              ReadCache::bool(), WriteCache::bool()) -> #dns_res{}
%% @doc Query the given Nameservers for a DNS record of Class, Type for
%% Name. Takes a Timeout in milliseconds. Returns results from the
%% `inet_db' RR cache if available and ReadCache is `true'. Writes
%% successful query answers to the cache if WriteCache is `true'.
%% @end
%% Query inet_db cache.
lookup(Name, Class = in, Type, Servers, Timeout,
       _ReadCache = true, WriteCache) ->
    case lookup_cache(Class, Name, Type) of
      {ok, Answer} -> {ok, Answer};
      {error, nxdomain} ->
	  lookup(Name, Class, Type, Servers, Timeout, false, WriteCache)
    end;
%% Perform DNS lookup and write results to inet_db cache
lookup(Name, Class, Type, Servers, Timeout,
       _ReadCache, _WriteCache = true) ->
    case lookup(Name, Class, Type, Servers, Timeout, false, false) of
      {ok, Rec} -> cache_lookup(Rec), Rec;
      Else -> Else
    end;
%% Lookup via DNS
lookup(Name, Class, Type, Servers, Timeout,
       _ReadCache = false, _WriteCache = false) ->
    inet_res:nnslookup(Name, Class, Type, Servers, Timeout).

%% @private
%% Process options list
expand_options(Opts) ->
    proplists:expand(option_expansions(), Opts).
%% @private
option_expansions() ->
    [{defaults, [{servers, resolvers()},
		 {timeout, timer:seconds(5)},
		 {read_cache, false},
		 {write_cache, false}]}
     ,{read_cached, [{servers, resolvers()},
                     {timeout, timer:seconds(5)},
                     {read_cache, true},
                     {write_cache, false}]}
    ].

%% @spec simplify(Result) -> {error, Reason::term()} | [simple_rr()]
%% where Result = {ok, #dns_rec{}} | {error, Reason::term()}
%% @doc Simplify the records returned from lookup to fixed-format
%% tuples instead of records.
%% @end
simplify({ok, #dns_rec{anlist=A}}) ->
    {ok, [{Name, Type, Data, [{ttl, TTL},{class,Class}]}
          || #dns_rr{domain=Name,class=Class,
                     type=Type,data=Data,
                     ttl=TTL} <- A]};
simplify(E) -> E.

%%====================================================================
%% Cache API
%%====================================================================

%% @spec lookup_cache(query_class(), Name::string(), query_type()) ->
%%  {ok, #dns_rec{}} | {error, Reason::term()}
%% @doc Query `inet_db' RR cache. Converts the `inet_db' `#hostent{}' respone
%%  into a fake `#dns_rec{}'.
lookup_cache(Class, Name, Type) ->
    case inet_db:getbyname(Name, Type) of
      {ok, #hostent{h_addr_list=Answers}} ->
	  %% Convert hostents to fake dns records
	  RRs = [#dns_rr{domain=Name, class=Class,
			 type=Type, ttl=cached,
			 data=D}
		 || D <- Answers],
	  {ok, #dns_rec{header=cached, anlist=RRs}};
        Else -> Else
    end.

%% @spec cache_lookup(#dns_rec{}) -> ok
%% @doc Store dns answer resource records in inet_db cache
cache_lookup(#dns_rec{anlist=RRs})  ->
    lists:foreach(fun inet_db:add_rr/1, RRs),
    ok.

%%====================================================================
%% Utility API
%%====================================================================

%% @spec resolvers() -> [resolver()]
%% @doc Returns a list of name servers that can be used as recursive
%% resolvers. Resolvers taken from the `inet_db' `namserver' setting.
resolvers() ->
    inet_db:res_option(nameserver).

%% @spec find_soa(Name::string()) -> {ok, {SoaName::string(), #dns_rr{}}} |
%% {error, Reason::term()}
%% @doc Recursively climb the ancestry of a domain name to find the
%% most specific SOA. (Closest domain delegation/authority)
%% Given `foo.bar.baz.com' would try `foo.bar.baz.com' then `bar.baz.com'
%% then `baz.com' and so on.
find_soa(Name) ->
    Components = string:tokens(Name, "."),
    find_soa2(Components).

%% @private
find_soa2([]) ->
    {error, nxdomain};
find_soa2(Components) ->
    Name = string:join(Components,"."),
    case lookup(Name, soa) of
        {ok, #dns_rec{anlist=[RR = #dns_rr{domain=Name,type=soa}]}} ->
            {ok, {Name, RR}};
        {error, nxdomain} ->
            find_soa2(tl(Components));
        Else -> Else
    end.

%% @spec domain_exists(DomainName::string()) -> bool()
%% @doc Returns `true' if a name exists and has an SOA record.
domain_exists(Name) ->
    case lookup(Name, soa) of
        {ok, #dns_rec{anlist=[#dns_rr{domain=Name,type=soa}]}} ->
            true;
        {error, nxdomain} ->
            false;
        Else -> erlang:error(Else)
    end.

%% @spec parse_resolv(FileName::string()) -> [ip_address()]
%% @doc Returns a list of nameservers from a POSIX style `resolv.conf' file.
parse_resolv(File) ->
    {ok, Rs} = inet_parse:resolv(File),
    [NS || {nameserver,NS} <- Rs].

%% @spec nameservers(Domain::string()) -> [resolver()]
%% @equiv nameservers(Domain, [read_cached])
nameservers(Domain) -> nameservers(Domain, [read_cached]).

%% @spec nameservers(Domain::string(), query_options()) -> [resolver()]
%% @doc Retrieve the names and addresses of the authoritative
%% nameservers for Domain. Will perform one NS lookup if the
%% resolving nameserver returns the address records for the queried
%% records, otherwise will perform 1 + N lookups (N = number of NS
%% records for Domain). Takes a list of query option that will be used
%% for NS lookups.
nameservers(Domain, Options) ->
    case lookup(Domain, ns, Options) of
        E = {error, _} -> E;
        {ok, #dns_rec{anlist=RRs, arlist=AR}} ->
            Names = [Name
                     || #dns_rr{data=Name,type=ns,domain=D} <- RRs,
                       D =:= Domain],
            Additional = [Addr || #dns_rr{data=Addr,type=a,domain=Name} <- AR,
                                  lists:member(Name, Names)],
            if length(Additional) > 0 ->
                    [nameserver_address(Addr) || Addr <- Additional];
               true ->
                    [nameserver_address(RR) || RR <- RRs]
            end
    end.

%% @spec nameserver_address(address()) -> resolver() | error
%% @doc
%% Convert a variety of nameserver address specifications into a
%% host-port tuple for use with `gen_udp' and `inet_res'.
%% @end
%% @todo Should take/respect query options instead of calling inet:getaddr/2.
nameserver_address(IP = {_, _, _, _}) -> {IP,53};
nameserver_address(Addr = {{_, _, _, _}, _}) -> Addr;
nameserver_address(Name) when is_list(Name) ->
    case inet:getaddr(Name, inet) of
        {ok, IP} -> {IP, 53};
        _ -> error
    end;
nameserver_address(#dns_rr{data=Name,type=ns}) ->
    nameserver_address(Name).

%% @spec direct_lookup(Name::string(), query_type(), Domain::string())
%% -> #dns_res{}
%% @doc Lookup the given Name/Type against the authoritative NSs for
%% Domain. This will bypass nxdomain caching and should result in
%% quicker recognition of changed/added records.
%% This is mainly intended for checking if people have setup a set of
%% DNS records correctly (say for a white-label hosting service).
direct_lookup(Name, Type, Domain) ->
    Servers = [{A,53} || {_NS, A} <- nameservers(Domain)],
    lookup(Name, Type, [{servers, Servers}]).

%%====================================================================
%% inet record manipulation and access API
%%====================================================================

%% @private
info(Field, Rec) ->
    Fields = fields(Rec),
    FieldIdx = lists:zip(Fields, lists:seq(2,length(Fields)+1)),
    element(proplists:get_value(Field,FieldIdx), Rec).

%% @private
fields(#dns_rec{}) -> fields(dns_rec);
fields(dns_rec) -> record_info(fields, dns_rec);
fields(#dns_rr{}) -> fields(dns_rr);
fields(dns_rr) -> record_info(fields, dns_rr).

%% @private
to_proplist(R) ->
    Keys = fields(R),
    Values = tl(tuple_to_list(R)),
    lists:zip(Keys,Values).

%%====================================================================
%% Unit Tests
%%====================================================================
%% @todo Add more unit tests.

%% @private
nameserver_address_test_() ->
    T = [{{1,2,3,4}, {{1,2,3,4},53}},
         {{{1,2,3,4},53}, {{1,2,3,4},53}},
         {#dns_rr{data={1,2,3,4},type=ns}, {{1,2,3,4},53}},
         {"localhost", {{127,0,0,1},53}},
         {#dns_rr{data="localhost",type=ns}, {{127,0,0,1},53}}],
    [ ?_assertMatch(C when C =:= B, nameserver_address(A))
      || {A,B} <- T].
