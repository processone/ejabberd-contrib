-xml(pubsub_serverinfo,
     #elem{name = <<"serverinfo">>,
	   xmlns = <<"urn:xmpp:serverinfo:0">>,
	   module = pubsub_serverinfo_codec,
	   result = {pubsub_serverinfo, '$domain'},
	   refs = [#ref{name = pubsub_serverinfo_domain,
	                label = '$domain',
	                min = 0}]}).

-xml(pubsub_serverinfo_domain,
     #elem{name = <<"domain">>,
	   xmlns = <<"urn:xmpp:serverinfo:0">>,
	   module = pubsub_serverinfo_codec,
	   result = {pubsub_serverinfo_domain, '$name', '$remote_domain'},
	   attrs = [#attr{name = <<"name">>,
	                  label = '$name',
	                  required = true}],
	   refs = [#ref{name = pubsub_serverinfo_federation,
	                label = '$remote_domain',
	                min = 0, max = 1}]}).

-xml(pubsub_serverinfo_federation,
     #elem{name = <<"federation">>,
	   xmlns = <<"urn:xmpp:serverinfo:0">>,
	   module = pubsub_serverinfo_codec,
	   result = '$remote_domain',
	   refs = [#ref{name = pubsub_serverinfo_remote_domain,
	                label = '$remote_domain',
	                min = 0}]}).

-xml(pubsub_serverinfo_remote_domain,
     #elem{name = <<"remote-domain">>,
	   xmlns = <<"urn:xmpp:serverinfo:0">>,
	   module = pubsub_serverinfo_codec,
	   result = {pubsub_serverinfo_remote_domain, '$name', '$type'},
	   attrs = [#attr{name = <<"name">>,
	                  label = '$name',
	                  required = true}],
	   refs = [#ref{name = pubsub_serverinfo_connection,
	                label = '$type',
	                min = 0}]}).

-xml(pubsub_serverinfo_connection,
     #elem{name = <<"connection">>,
	   xmlns = <<"urn:xmpp:serverinfo:0">>,
	   module = pubsub_serverinfo_codec,
	   result = '$type',
	   attrs = [#attr{name = <<"type">>,
	                  label = '$type',
	                  required = true,
	                  enc = {enc_enum, []},
	                  dec = {dec_enum, [[incoming, outgoing, bidi]]}}]}).
