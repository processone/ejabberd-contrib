	mod_webpresence - Presence on the Web

	Authors: Igor Goryachev, Badlop, runcom
	http://www.ejabberd.im/mod_webpresence


	DESCRIPTION
	-----------

This module allows any local user of the ejabberd server to publish his
presence information in the web.
This module is the succesor of Igor Goryachev's mod_presence.

Allowed output methods are
 * Icons (various themes available)
 * Status text
 * Raw XML
 * Avatar, stored in the user's vCard

No web server, database, additional libraries or programs are required.



	INSTALL
	-------

1. Compile the module
 * On Windows: build.bat
 * On other systems: ./build.sh

2. Copy ebin/mod_webpresence.beam to your ejabberd ebin directory.

3. Copy the directory data/pixmaps to a directory you prefer.

4. Edit ejabberd.cfg and add the HTTP and module definitions:

listen:
  -
    port: 5280
    module: ejabberd_http
    [...]
    request_handlers:
      "presence": mod_webpresence

modules:
  [...]
  mod_webpresence:
    pixmaps_path: "/path/to/pixmaps"

5. Restart ejabberd.
If problems appear, remember to always look first the ejabberd log files
ejabberd.log and sasl.log since they may provide some valuable information.


	CONFIGURABLE PARAMETERS
	-----------------------

host 
    Define the hostname of the service.
    You can use the keyword @HOST@.
    Default value: "webpresence.@HOST@"
access:
    Specify who can register in the webpresence service.
    Don't bother to specify 'all' because this module can only show presence of 
    local users.
    Default value: local
pixmaps_path:
    Take special care with commas and dots: if this module does not seem to work
    correctly, the problem may be that the configuration file has syntax errors.
    Remember to put the correct path to the pixmaps directory,
    and make sure the user than runs ejabberd has read access to that directory.
    Default value: "./pixmaps"
port:
    This informational option is used only when sending a message to the user.
    If you set a different port in the 'listen' section, set this option too.
    Default value: 5280
path:
    This informational option is used only when sending a message to the user.
    If you set a different path in the 'listen' section, set this option too.
    Default value: "presence"
baseurl:
    This informational option is used only when sending a message to the user
    and when building the JavaScript code.
    It is the base part of the URL of the webpresence HTTP content.
    If the option is not specified, it takes as default value: http://host:port/path/


	AUTOMATIC ENABLE
	----------------

If you want certain Jabber accounts to be automatically accepted,
without requiring the user to register in the service, you can user ACL+ACCESS.
The ACCESSNAME 'webpresence_auto' is available for that purpose.

In that case, all the output methods are enabled, the icon theme is 
'jsf-jabber-text' and RandomID is disabled.

The default behaviour is to not have automatic webpresence:
  access:
    webpresence_auto:
      all: deny

For example, if you want all the local users to be automatically enabled in the service:
  access:
    webpresence_auto:
      local: allow

Note that this ACCESS rule is only checked if the user is not registered. 
So, if the user registers and disables all output methods,
his registration prevails over your setup. If you want to ensure the users do not
register and disable output methods, you can use the Access configurable parameter.


	EXAMPLE CONFIGURATION
	---------------------

	Example 1
	---------

listen:
  -
    port: 5280
    module: ejabberd_http
    [...]
    request_handlers:
      "presence": mod_webpresence

modules:
  [...]
  mod_webpresence:
    pixmaps_path: "/path/to/pixmaps"


	Example 2
	---------

listen:
  -
    port: 80
    module: ejabberd_http
    [...]
    request_handlers:
      "status": mod_webpresence

modules:
  [...]
  mod_webpresence:
    host: "webstatus.@HOST@"
    access: local
    pixmaps_path: "/path/to/pixmaps"
    port: 80
    path: "status"
    baseurl: "http://www.example.org/status/"


	USAGE
	-----

The web-presence feature by default is switched off for every user. If
user wants to use it, he should register on service webpresence.example.org,
which is accessible from Service Discovery. 
There are several switches for web-presence:
 * Jabber ID: publish the presence in URIs that use the user's Jabber ID.
 * Random ID: publish the presence in URIs that use a Random ID.
 * XML: allow XML output.
 * Icon: allow icon output.
 * Avatar: allow Avatar output.

Login to an account on your ejabberd server using a powerful Jabber client.
Open the Service Discovery on your Jabber client, and you should see
a new service called "webpresence.example.org".
Try to register on it. A formulary appears allowing the user to 
allow image publishing, and XML publishing.

Once you enabled some of those options, 
on a web browser open the corresponding URI:
 * for XML output:
	http://example.org:5280/presence/jid/<user>/<server>/xml/ 
 * for image output:
	http://example.org:5280/presence/jid/<user>/<server>/image/
 * for image output with theme:
	http://example.org:5280/presence/jid/<user>/<server>/image/theme/<theme>/
 * for avatar output:
	http://example.org:5280/presence/jid/<user>/<server>/avatar/ 

If you want to show the image or text outputs of a specific resource, add /res/<resource>
to the URI:
  http://example.org:5280/presence/jid/<user>/<server>/text/res/<resource>
  http://example.org:5280/presence/jid/<user>/<server>/image/res/<resource>
  http://example.org:5280/presence/jid/<user>/<server>/image/theme/<theme>/res/<resource>

For output types image and avatar, you can append any string to a valid URI.
For example, you can use this URI:
  http://example.org:5280/presence/jid/<user>/<server>/image/theme/<theme>/myimage.jpeg
The response is exactly the same than the regular image/theme/<theme>/

If you don't want to reveal your Jabber ID, you can enable Random ID URI.
After the registration the user gets a message with his a pseudo-random ID.
The URI can be formed this way:
  http://example.org:5280/presence/rid/<rid>/image/
If the user forgets his Random ID, he can get another message by just registering again,
there is no need to change the values.
If the user wants to get a new Random ID, he must disable Random ID in the registration form,
and later enable Random ID again. A new Random ID will be generated for him.


	EXAMPLE PHP CODE
	----------------

This PHP script generates HTML code.
Thanks to Tobias Markmann and NoAlWin.

It assumes that the URI of the presence is:
  http://example.org:5280/presence/jid/tom/example.org

<?php
	$doc = new DOMDocument();
	$doc->load('http://example.org:5280/presence/jid/tom/example.org/xml');
	$presences = $doc->getElementsByTagName("presence");
	foreach ($presences as $presence) {
		echo "<p>";
		echo "<img src='http://example.org:5280/presence/jid/tom/example.org/avatar' style='display: block; margin-right: 10px;float: left;'/>";
		echo "<a href='xmpp:".$presence->getAttribute('user').'@'.$presence->getAttribute('server')."'>";
		echo "Tobias Markmann</a><br />";
		$resources = $presence->getElementsByTagName("resource");
		if($resources->length == 0){
			echo 'Unavailable';
		}else{
			foreach ($resources as $resource) {
				echo "<a href='xmpp:".$presence->getAttribute('user').'@'.$presence->getAttribute('server').'/'.$resource->getAttribute('name')."'>".$resource->getAttribute('name')."</a> &gt; ";
				switch($resource->getAttribute('show')){
					case 'chat':	echo 'Free for chat'; break;
					case 'xa':	echo 'Extended away'; break;
					case 'dnd':	echo 'Do not disturb'; break;
					default:	echo ucfirst($resource->getAttribute('show'));
				}
				if($resource->nodeValue){
					echo ": ".$resource->nodeValue;
				}
				echo "<br />";
			}
		}
		echo "</p>";
	}
?> 


	JAVASCRIPT CALLBACK
	-------------------

The JavaScript output supports cross-site AJAX calls.

Basically, it allows to tack on a callback parameter to presence
requests like so:
	http://example.org:5280/presence/jid/<user>/<server>/js?cb=doStuff
Which then gets fed back in the result as:
	var jabber_resources = [...]; doStuff();

The motivation for this is to work around browser restrictions in
cross-site scripting. You can use it by adding a new <script/> node to
the DOM at runtime, with the presence URL as its source. This will
cause doStuff() to be called when the presence information has
loaded. The end result is you can display real-time presence updates
on a different domain than your jabber server.
