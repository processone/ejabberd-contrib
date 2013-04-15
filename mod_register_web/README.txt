
	mod_register_web - Web to register account

	Homepage: http://www.ejabberd.im/mod_register_web
	Author: Badlop
	Requirements: ejabberd ejabberd 2.1.0 or higher


	DESCRIPTION
	-----------

This module provides a web page where users can register Jabber accounts,
change password and other related tasks.


	CONFIGURATION
	-------------

Add to ejabberd.cfg, 'modules' section the basic configuration:
{modules, [
  ...
  {mod_register_web,     []},
  ...
]}.


In the 'listen' section enable the web page:
{listen, [
  ...
  {5281, ejabberd_http, [
	tls,
    {certfile, "/etc/ejabberd/certificate.pem"},
    {request_handlers, [
      {["register"], mod_register_web}
    ]}
  ]},
  ...
]}.

In this example the page is served in https://example.org:5281/register/

Make sure to include the last / character in the URL.
Otherwise when you enter a subpage the URL will not be correct, 
for example: http://localhost:5281/new  --->  404 Not Found

This module supports CAPTCHA image to register a new account.
To enable this feature, configure the options captcha_cmd and captcha_host.
See the ejabberd Guide for more information about those options.

This module supports the option registration_watchers,
which works similarly to the mod_register option.


	FEATURE REQUESTS
	----------------

 * Allow configuration of the hardcoded "register" path in URL.
 * Enforce configurable ACL+ACCESS to register

 * Improve the default CSS to provide an acceptable look.
 * Option to use a custom CSS file.
 
 * Optionally registration request is only forwarded to admin, no account created.
 
 * Option to select which subpages are available

 * Store in a custom mnesia table: timestamp of account register and IP.
 * Use time limiter by IP like mod_register for: register, changepass.

 * Allow private email during register, and store in custom table.
 * Optionally require private email to register.
 * Optionally require email confirmation to register.
 
 * Allow to set a private email address anytime.
 * Allow to recover password using the private email to confirm (see mod_passrecover).

 * Optionally require invitation
