
	mod_cron - Execute scheduled commands

	http://www.ejabberd.im/mod_cron
	Author: Badlop
	Requirements: ejabberd git master


This module allows advanced ejabberd administrators to schedule commands for
periodic and automatic execution. This module is a similar concept than the
*nix's cron program. Obviously, the admin must know in advance which module,
function and arguments to use, so this module is not intended for starting
administrators.

Each time a scheduled task finish its execution, a message is printed in the
ejabberd log file.


	BASIC CONFIGURATION
	===================

Add the module to your ejabberd.yml, on the modules section:
modules:
  mod_cron: {}


	TASK SYNTAX
	===========

Each task is described with five elements:
* Time is an integer.
* Units indicates the time unit you use. It can be: seconds, minutes, hours, days.
* Module and * Function are the exact call you want to schedule.
* Arguments is an erlang list of arguments inside the characters "> ."


	EXAMPLE TASKS
	=============

Example configuration with some tasks:
modules:
  mod_cron:
    tasks:
      - time: 3
        units: hours
        module: mnesia
        function: info
        arguments: "> []."
      - time: 10
        units: seconds
        module: ejabberd_auth
        function: try_register
        arguments: "> [\"user1\", \"localhost\", \"somepass\"]."


	EJABBERD COMMANDS
	=================

This module provides two new commands that can be executed using ejabberdctl:
* cron_list: list scheduled tasks
* cron_del taskid: delete this task from the schedule


	WEB ADMIN
	=========

This module provides a page in the Host section of the Web Admin.
Currently that page only allows to view the tasks scheduled for that host.
