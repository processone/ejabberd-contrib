
	mod_cron - Execute scheduled commands

	http://www.ejabberd.im/mod_cron
	Author: Badlop
	Requirements: ejabberd trunk SVN 1635 or newer


This module allows advanced ejabberd administrators to schedule commands for
periodic and automatic execution. This module is a similar concept than the
*nix's cron program. Obviously, the admin must know in advance which module,
function and arguments to use, so this module is not intended for starting
administrators.

Each time a scheduled task finish its execution, a message is printed in the
ejabberd log file.


	BASIC CONFIGURATION
	===================

Add the module to your ejabberd.cfg, on the modules section:
{modules, [
  ...
  {mod_cron, []},
  ...
]}.


	TASK SYNTAX
	===========

Each task is described using a tuple with this syntax:
  {Time, Time_units, Module, Function, Arguments}
Where:
  Time is an integer.
  Time_units indicates the time unit you use. It can be: seconds, minutes, hours, days.
  Module and Function are the exact call you want to schedule.
  Arguments is a list of arguments. It can be emtpy.

For example, let's define some dummy tasks:
 * Every 3 hours, print on the log file some info about mnesia:
    {3, hours, mnesia, info, []}

 * Every day, try to register certain account:
    {1, days, ejabberd_auth, try_register, ["tommy", "jabber.example.org", "MyP455WorD"]}


	TASKS ON EJABBERD.CFG
	=====================

Now that you know how to define new tasks, you can add the ones you want on ejabberd.cfg
For example:
{modules, [
  ...
  {mod_cron, [{tasks, [
    {3, hours, mnesia, info, []},
    {1, days, ejabberd_auth, try_register, ["aaa", "atenea", "aaaaaa"]}
  ]}]},
  ...
]}.


	EJABBERD COMMANDS
	=================

This module provides two new commands that can be executed using ejabberdctl:
 * cron-list: list scheduled tasks
 * cron-del taskid: delete this task from the schedule


	WEB ADMIN
	=========

This module provides a page in the Host section of the Web Admin.
Currently that page only allows to view the tasks scheduled for that host.
