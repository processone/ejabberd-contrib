

	mod_multicast - Extended Stanza Addressing (XEP-0033) support

	Homepage: http://ejabberd.jabber.ru/mod_multicast
	Author: Badlop
	Module for ejabberd SVN


*****************************************
**
**    IMPORTANT!!
**
** This code is now hosted and developed in this Git repository and branch:
** https://git.process-one.net/~badlop/ejabberd/badlop-ejabberd/commits/multicast-2.1.x
**
*****************************************


	DESCRIPTION
	-----------

This module implements Extended Stanza Addressing (XEP-0033).

The development of this module is included on a Google Summer of Code 2007 project.


	INSTALL
	-------

1. Compile the module.
2. Copy the binary files to ejabberd ebin directory.
3. Edit ejabberd.cfg and add the module to the list of modules:
  {mod_multicast, []},
4. Start ejabberd.


	CONFIGURABLE PARAMETERS
	-----------------------

host 
    Define the hostname of the service.
    Default value: "multicast.SERVER"
access:
    Specify who can send packets to the multicast service.
    Default value: all
limits:
    Specify a list of custom limits which override the default ones defined 
    in XEP-0033.
    Limits are defined with this syntax: {Sender_type, Stanza_type, Number}
    Where:
      Sender_type can have values: local or remote.
      Stanza_type can have values: message or presence.
      Number can be a positive integer or the key word infinite.
    Default value: []


	EXAMPLE CONFIGURATION
	---------------------

% Only admins can send packets to multicast service
{access, multicast, [{allow, admin}, {deny, all}]}.

% If you want to allow all your users:
%{access, multicast, [{allow, all}]}.

% This allows both admins and remote users to send packets,
% but does not allow local users
%{acl, allservers, {server_glob, "*"}}.
%{access, multicast, [{allow, admin}, {deny, local}, {allow, allservers}]}.


{modules, [
  ...
  {mod_multicast, [
     %{host, "multicast.example.org"},
     {access, multicast},
     {limits, [
       {local, message, 40},
       {local, presence, infinite},
       {remote, message, 150}
     ]}
  ]},
  ...
]}.


	TO DO
	-----

Tasks to do:
 - Consider anti-spam requirements
 
Feature requests:
 - GUI with FORMS to allow users of non-capable clients to write XEP-33 packets easily

Could use mod_multicast somehow:
 - mod_pubsub/mod_pep
 - mod_irc
