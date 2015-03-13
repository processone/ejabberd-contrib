
	mod_cron - Execute scheduled commands

	http://www.ejabberd.im/mod_cron
	Author: Badlop


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
* Arguments is an array.  Strings will be converted to binaries.
* timer_type is one of 'fixed' or 'interval'.  Fixed timers occur at a fixed time
  after the [minute|hour|day] e.g. every hour on the 5th minute (1:05PM, 2:05PM etc)
  interval timers occur every interval (starting on an even unit) e.g. every 10 minutes
  starting at 1PM, 1:10PM, 1:20PM etc.  

  Fixed timers are the equivalent of unix cron's comma syntax e.g. "2 * * *" and interval
  timers are the / syntax e.g. "*/5 * * *".

  Default timer_type is interval.

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
        arguments: {}
        timer_type: fixed
      - time: 10
        units: seconds
        module: ejabberd_auth
        function: try_register
        arguments: 
          - "user1"
          - "localhost"
          - "somepass"
        timer_type: interval


	EJABBERD COMMANDS
	=================

This module provides two new commands that can be executed using ejabberdctl:
* cron_list: list scheduled tasks
* cron_del taskid: delete this task from the schedule


	WEB ADMIN
	=========

This module provides a page in the Host section of the Web Admin.
Currently that page only allows to view the tasks scheduled for that host.
