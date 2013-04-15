

	mod_ctlextra - Additional commands for ejabberdctl

	Homepage: http://www.ejabberd.im/mod_ctlextra
	Author: Badlop
	Module for ejabberd 2.0.*


	IMPORTANT
	=========

This module will only receive bugfixes.  All the features provided by
this module are also provided by the new module mod_admin_extra.

If you are using ejabberd 2.1.0 or newer, use mod_admin_extra instead.
It provides generic ejabberd
commands that can be executed not only with ejabberdctl, but also with
other ejabberd-command frontends.


	CONFIGURATION
	=============

Add the module to your ejabberd.cfg, on the modules section:
{modules, [
  ...
  {mod_ctlextra, []},
  ...
]}.


	USAGE
	=====

Now you have several new commands in ejabberdctl.

Description of some commands:

 - vcard-*
   Example: ejabberdctl eja@host vcard-get joe myjab.net email

 - pushroster*
   The file used by 'pushroster' and 'pushroster-all' must be placed:
     - Windows: on the directory were you installed ejabberd: 
       'C:/Program Files/ejabberd'
     - Other OS: on the same directory where the .beam files are.
   Example content for the roster file:
   [{"bob", "example.org", "workers", "Bob"},
    {"mart", "example.org", "workers", "Mart"},
    {"Rich", "example.org", "bosses", "Rich"}].

 - srg-create
   If you want to put a group Name with blankspaces, use the characters 
   "' and '" to define when the Name starts and ends.
   For example:
   ejabberdctl srg-create g1 example.org "'Group number 1'" this_is_g1 g1

 - ban-account

   This command kicks all the connected sessions of the account from the
   server.  It also changes his password to another randomly
   generated, so he can't login anymore unless a server administrator
   changes him again the password.

   It is possible to define the reason of the ban.  The new password
   also includes the reason and the date and time of the ban.

   For example, if this command is called:
     ejabberdctl vhost example.org ban-account boby Spammed several MUC rooms
   then the sessions of the local account which JID is boby@example.org
   will be kicked, and its password will be set to something like this:
     BANNED_ACCOUNT--20080425T21:45:07--2176635--Spammed_several_MUC_rooms

