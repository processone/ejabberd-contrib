

	mod_muc_admin - Administrative features for MUC

	NOTE: This module is included in ejabberd since 15.04

	Homepage: http://www.ejabberd.im/mod_muc_admin
	Author: Badlop

This module implements several ejabberd commands that can be
executed using ejabberdctl.

It also implements Web Admin pages to view the list of existing
rooms.


	CONFIGURATION
	=============

Add the module to your ejabberd.yml, on the modules section:
modules:
  mod_muc_admin: {}


	EJABBERD COMMANDS
	=================

Description of some commands:

 - muc-unusued-*
   Those commands related to MUC require an ejabberd version newer than 1.1.x.
   The room characteristics used to decide if a room is unusued:
    - Days since the last message or subject change:
        greater or equal to the command argument
    - Number of participants: 0
    - Persistent: not important
    - Has history: not important
    - Days since last join, leave, room config or affiliation edit:
        not important
    - Just created: no
   Note that ejabberd does not keep room history after a module restart, so
   the history of all rooms is emtpy after a module or server start.

