
	mod_logxml - Log XMPP packets to XML file

	Homepage: http://www.ejabberd.im/mod_logxml
	Author: Badlop
	Module for ejabberd git master


	DESCRIPTION
	-----------

This module sniffs all the XMPP traffic send and received by ejabberd,
both internally and externally transmitted. It logs the XMPP packets
to a XML formatted file. It's posible to filter transmitted packets 
by orientation, stanza and direction. It's possible to configure the 
file rotation rules and intervals.

This module reuses code from mod_log_forensic, mod_stats2file, mod_muc_log


	CONFIGURATION
	-------------

stanza: 
    Log packets only when stanza matches
    Default value: [iq, message, presence, other]
direction: 
    Log packets only when direction matches
    Default value: [internal, vhosts, external]
orientation: 
    Log packets only when orientation matches
    Default value: [send, revc]
logdir: 
    Base filename, including absolute path
    Default value: "/tmp/jabberlogs/"
timezone:
    The time zone for the logs is configurable with this option. 
    Allowed values are 'local' and 'universal'.
    With the first value, the local time, 
    as reported to Erlang by the operating system, will be used. 
    With the latter, GMT/UTC time will be used. 
    Default value: local
show_ip: 
    If the IP address of the local user should be logged to file.
    Default value: false
rotate_days: 
    Rotate logs every X days
    Put 'no' to disable this limit.
    Default value: 1
rotate_megs: 
    Rotate when the logfile size is higher than this, in megabytes.
    Put 'no' to disable this limit.
    Default value: 10
rotate_kpackets: 
    Rotate every *1000 XMPP packets logged
    Put 'no' to disable this limit.
    Default value: 10
check_rotate_kpackets: 
    Check rotation every *1000 packets
    Default value: 1


	EXAMPLE CONFIGURATION
	---------------------

In ejabberd.yml, in the modules section, add the module. For example:

modules:
  mod_logxml:
     stanza:
       - iq
       - other
     direction:
       - external
     orientation:
       - send
       - recv
     logdir: "/tmp/logs/"
     timezone: universal
     show_ip: false
     rotate_days: 1
     rotate_megs: 100
     rotate_kpackets: no
     check_rotate_kpackets: 1


	FORMAT OF XML
	-------------

XMPP packets are enclosed in <packet>, with attributes:
  or: orientation of the packet, either 'send' or 'recv'
  ljid: local JID of the sender or receiver, depending on the orientation
  ts: timestamp when the packet was logged
