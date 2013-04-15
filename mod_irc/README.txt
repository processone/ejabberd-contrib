
	mod_irc - IRC transport

	Author: Alexey Shchepin
	Requires: GNU Iconv 1.8 or higher. Not needed on systems with GNU Libc.
	http://www.ejabberd.im/mod_irc


	DESCRIPTION
	-----------

This module is an IRC transport that can be used to join channels on
IRC servers.

This module was originally included in ejabberd releases.
It is not included since ejabberd 3.0.0.


	INSTALL
	-------

1. Compile the module
 * On Windows: build.bat
 * On other systems: ./build.sh; make

2. Copy the beam files from ebin directory to your ejabberd ebin directory.

3. Copy iconv_erl.so from ebin directory to your ejabberd binary
system libraries directory.
It may be something like /lib/ejabberd/priv/lib/

4. Edit ejabberd.cfg and add the module definition:
{modules, [
  {mod_irc, []},
  ...
]}.

5. Restart ejabberd.
If problems appear, remember to always look first the ejabberd log files
ejabberd.log and sasl.log since they may provide some valuable information.


	CONFIGURABLE PARAMETERS
	-----------------------

host: This option defines the Jabber ID of the service. If the host
    option is not specified, the Jabber ID will be the hostname of the
    virtual host with the prefix ‘irc.’. The keyword "@HOST@" is
    replaced at start time with the real virtual host name.

access: This option can be used to specify who may use the IRC
    transport (default value: all).

default_encoding: Set the default IRC encoding (default value:
    "koi8-r").


	EXAMPLE CONFIGURATION
	---------------------

	Example 1
	---------

In the first example, the IRC transport is available on (all) your
virtual host(s) with the prefix ‘irc.’. Furthermore, anyone is able to
use the transport. The default encoding is set to "iso8859-15".

{modules, [
  {mod_irc, [{access, all},
             {default_encoding, "iso8859-15"}]},
  ...
]}.

	Example 2
	---------

In next example the IRC transport is available with JIDs with prefix
irc-t.net. Moreover, the transport is only accessible to two users of
example.org, and any user of example.com:

{acl, paying_customers, {user, "customer1", "example.org"}}.
{acl, paying_customers, {user, "customer2", "example.org"}}.
{acl, paying_customers, {server, "example.com"}}.

{access, irc_users, [{allow, paying_customers}, {deny, all}]}.

{modules, [
  {mod_irc, [{access, irc_users},
             {host, "irc.example.net"}]},
  ...
]}.


	USAGE
	-----

How to use the IRC transport:

* A Jabber client with ‘groupchat 1.0’ support or Multi-User Chat
  support (XEP-0045) is necessary to join IRC channels.

* An IRC channel can be joined in nearly the same way as joining a
  Jabber Multi-User Chat room. The difference is that the room name
  will be ‘channel%irc.example.org’ in case irc.example.org is the IRC
  server hosting ‘channel’. And of course the host should point to the
  IRC transport instead of the Multi-User Chat service.

* You can register your nickame by sending ‘IDENTIFY password’ to
  nickserver!irc.example.org@irc.jabberserver.org.

* Entering your password is possible by sending ‘LOGIN nick password’
  to nickserver!irc.example.org@irc.jabberserver.org.

* The IRC transport provides Ad-Hoc Commands (XEP-0050) to join a
  channel, and to set custom IRC username and encoding.

* When using a popular Jabber server, it can occur that no connection
  can be achieved with some IRC servers because they limit the number
  of conections from one IP.


