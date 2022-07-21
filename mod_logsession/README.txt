
	mod_logsession - Log session connections to file

	Requirements: ejabberd 19.08 or higher
	Homepage: http://www.ejabberd.im/mod_logsession
	Author: Badlop


	DESCRIPTION
	-----------

This module is intended to log in a text file the session connections.
Right now it only logs the forbidden connection attempts and the
failed authentication attempts.
Each vhost is logged in a different file.

Note: to log the failed authentication attempts, you need to patch ejabberd.


	CONFIGURABLE PARAMETERS
	-----------------------

sessionlog:
  Define the name of log files, or set to 'auto'.
  The keyword @HOST@ is substituted with the name of the vhost.
  If set to 'auto', it will store in the ejabberd log path
  with the filename "session_@HOST@.log"
  Default value: auto


	EXAMPLE CONFIGURATION
	---------------------

modules:
  ...
  mod_logsession:
    sessionlog: "/var/log/ejabberd/session_@HOST@.log"
  ...

With that configuration, if the server has three vhosts:
  "localhost", "example.org" and "example.net",
then the forbidden accesses will be logged in the files:
  /var/log/ejabberd/session_localhost.log
  /var/log/ejabberd/session_example.org.log
  /var/log/ejabberd/session_example.net.log


	FORMAT OF LOG
	-------------

The content of the file is the date and time of the attempted login
and the JID of the denied user.

For example:
2008-01-08 12:20:50 Forbidden session for tron@localhost/teeest
2008-01-08 12:36:01 Forbidden session for baduser@localhost/aaa22
2010-04-02 17:21:37 Failed authentication for someuser@localhost from 127.0.0.1 port 58973
2010-04-02 17:25:20 Failed authentication for badlop@localhost from 127.0.0.1 port 45842


	REOPEN LOG FILES
	----------------

This module provides an ejabberd command to reopen the log file
of a host where the module is enabled.

Example usage:
  ejabberdctl reopen-seslog localhost
  ejabberdctl reopen-seslog jabber.example.org

