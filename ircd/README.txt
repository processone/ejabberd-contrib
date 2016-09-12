

		***************
		  PLEASE NOTE
		***************

	This module does NOT work
	with ejabberd 13 or newer.

		***************


	ircd  -  IRC-to-XMPP interface

	Author:
	  Magnus Henoch
	  xmpp:legoscia@jabber.cd.chalmers.se,
	  mailto:henoch@dtek.chalmers.se
	Homepage:
	  http://www.dtek.chalmers.se/~henoch/text/ejabberd-ircd.html
	Requirements:
	  ejabberd trunk SVN 1631 or newer


	DESCRIPTION
	===========

This is an IRC server frontend to ejabberd.  It supports a subset of
the IRC protocol, allowing IRC users to use a subset of Jabber MUC
functions.  Users log in with their username and password, just as if
they were Jabber users.  Therefore, configuring the IRC interface to
use an anonymous authentication backend is probably what users expect.
Channel names are translated to MUC rooms on a particular MUC service.

The most obvious missing functions in this module are operator actions
and a command to list channels.


	CONFIGURATION
	=============

Something like this should be inserted in the "listen" section of the
configuration file:

{listen, [
  ...
  {6667, ejabberd_ircd,    [{access, c2s},
			    {host, "example.org"},
			    {muc_host, "conference.example.org"},
			    {encoding, "utf-8"},
			    {mappings,
			    [{"#esperanto", "esperanto@conference.jabber.org"}]} ]},
  ...
]}.

Configurable module options:
  access: ACL matching users allowed to use the IRC backend.
  host: hostname part of the JIDs of IRC users.
  muc_host: MUC service hosting IRC "channels".
  encoding: encoding that IRC users are expected to use.
  mappings: optional list of mappings from channel names to MUC rooms
    on other MUC services.


	AUTHENTICATION
	==============

The IRC client needs to login in ejabberd. If the 'internal' auth
method is enabled, then the IRC client must provide the username and
password of an existing Jabber account.

If you want to allow an IRC client to join in MUC rooms without
requiring authentication, you can enable anonyous authentication in
ejabberd.

Note that this module doesn't do SASL ANONYMOUS authentication.  This
means that to use anonymous authentication, the "anonymous_protocol"
option needs to be either "login_anon" or "both".

For example, you can define a new Jabber virtual host used only for
anonymous authentication by ejabberd_ircd:

{hosts, ["example.org", "anonymous.example.org"]}.
{host_config, "anonymous.example.org",
	      [{auth_method, anonymous},
	       {anonymous_protocol, both}]}.
{listen, [
  ...
  {6667, ejabberd_ircd,    [{access, c2s},
			    {host, "anonymous.example.org"},
			    {muc_host, "conference.example.org"},
			    {encoding, "utf-8"} ]},
  ...
]}.
