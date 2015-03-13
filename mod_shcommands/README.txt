
	mod_shcommands - Execute shell commands

	Author: Badlop
	http://ejabberd.im/mod_shcommands




                *****************

                  W A R N I N G

                *****************


         USE THIS MODULE AT YOUR OWN RISK

    This module allows ejabberd administrators 
        to remotely execute shell commands 
          which could compromise both 
    the ejabberd server and the whole machine. 


                *****************

                  W A R N I N G

                *****************




	DESCRIPTION
	===========

This module provides the ejabberd server administrator a method to remotely
execute shell commands in the ejabberd server.

It provides a page in the ejabberd Web Admin.
Only the administrators of the whole server can access this page.

Three types of commands are possible:
    * ejabberd_ctl: makes a call to ejabberd_ctl
    * erlang shell: executes an erlang command
    * system shell: executes a command on the system shell
The result of the execution will be shown.

In the system shell, only non-interactive commands will work correctly, 
for example this will work:
  ps -all
Don't use commands that start an interactive mode:
 DON'T TRY THIS: top
 DON'T TRY THIS: vim readme.txt

This module does not check if the commands are dangerous or problematic,
so this module is only recommended for experienced ejabberd and Erlang/OTP
administrators.



	BASIC CONFIGURATION
	===================

Add the module to your ejabberd.yml, on the modules section:
modules:
  mod_shcommands: {}
