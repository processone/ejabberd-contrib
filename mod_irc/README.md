## mod_irc

This module is an IRC transport that can be used to join channels on IRC
servers.

End user information:

-   A XMPP client with ‘groupchat 1.0’ support or Multi-User Chat
	support ([`XEP-0045`][72]) is
	necessary to join IRC channels.

-   An IRC channel can be joined in nearly the same way as joining a
	XMPP Multi-User Chat room. The difference is that the room name will
	be ‘channel%`irc.example.org`’ in case `irc.example.org` is the IRC
	server hosting ‘channel’. And of course the host should point to the
	IRC transport instead of the Multi-User Chat service.

-   You can register your nickame by sending ‘IDENTIFY password’ to
	`nickserver!irc.example.org@irc.jabberserver.org`.

-   Entering your password is possible by sending ‘LOGIN nick
	password’
	to `nickserver!irc.example.org@irc.jabberserver.org`.

-   The IRC transport provides Ad-Hoc Commands
	([`XEP-0050`][73]) to join a
	channel, and to set custom IRC username and encoding.

-   When using a popular XMPP server, it can occur that no connection
	can be achieved with some IRC servers because they limit the number
	of connections from one IP.

Options:

**`host: HostName`**:   This option defines the Jabber ID of the service. If the `host`
	option is not specified, the Jabber ID will be the hostname of the
	virtual host with the prefix ‘`irc.`’. The keyword “@HOST@” is
	replaced at start time with the real virtual host name.

**`db_type: mnesia|sql|riak`**:   Define the type of storage where the module will create the tables and store user information. The default is the storage defined by the global option `default_db`, or `mnesia` if omitted. If `sql` or `riak` value is defined, make sure you have defined the database, see [database](#database-and-ldap-configuration).

**`access: AccessName`**:   This option can be used to specify who may use the IRC transport
	(default value: `all`).

**`default_encoding: Encoding`**:   Set the default IRC encoding. Default value: `iso8859-1`

Examples:

-   In the first example, the IRC transport is available on (all) your
	virtual host(s) with the prefix ‘`irc.`’. Furthermore, anyone is
	able to use the transport. The default encoding is set to
	“iso8859-15”.

				
		modules:
		  ...
		  mod_irc:
		    access: all
		    default_encoding: "iso8859-15"
		  ...

-   In next example the IRC transport is available with JIDs with prefix
	`irc-t.net`. Moreover, the transport is only accessible to two users
	of `example.org`, and any user of `example.com`:

				
		acl:
		  paying_customers:
		    user:
		      - "customer1": "example.org"
		      - "customer2": "example.org"
		    server: "example.com"

		access_rules:
		  irc_users:
		    - allow: paying_customers

		modules:
		  ...
		  mod_irc:
		    access: irc_users
		    host: "irc.example.net"
		  ...
