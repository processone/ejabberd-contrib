

		***************
		  PLEASE NOTE
		***************

	This module does NOT work
	with ejabberd 13 or newer.

		***************


mod_openid 
Transform the Jabber Server in an openid provider.
(http://openid.net/)

Author: Olivier Goffart <ogoffart@kde.org>

Motivation:
There are already severals existing openid provider that uses the JabberId as id. 
( http://openid.xmpp.za.net/ http://xmppid.net/ )
But none of them are open source.
The idea is that having the openid server in the same place as the jabber server reduce 
the size of the security chain we have to trust.   
Instead of trusting both the jabber server and the openid provider, we can trust only 
the Jabber server.


Status:
Currently, the implementation just ask for the jabber password. 
Some security function are also lacking.
The plan was to use something similair to XEP-0070

How it works:
Add in your ejabberd.cfg
{listen, [  ...
          {5280, ejabberd_http,    [http_poll, web_admin, {request_handlers , [{["openid"],mod_openid }]}]} ,

Then your open id is    http://server.org:5280/openid/user@server.org
Hopelifully it should be possible to have more nice-looking urls.


Future:
I have no plan to continue working on it.  Feel free to take over.
I'd be happy to reply to questions.

