
	mod_filter - Flexible Filtering by Server Policy

	Author: Magnus Henoch <henoch@dtek.chalmers.se>
	Copyright (C) 2005 Magnus Henoch



This module allows the admin to specify packet filtering rules using ACL and ACCESS.


	CONFIGURATION
	=============

To use this module, follow the general build instructions.
You can modify the default module configuration file like this:

To enable the module:
modules:
  mod_filter: {}

And you must also add the default access rules:
access_rules:
  mod_filter:
    - allow: all
  mod_filter_presence:
    - allow: all
  mod_filter_message:
    - allow: all
  mod_filter_iq:
    - allow: all

The configuration of rules is done using ejabberd's ACL and ACCESS,
so you should also study the corresponding section on ejabberd guide.
You can find here several examples that may help you to understand how it works.


	EXAMPLE 1
	=========

access_rules:
  mod_filter_presence:
    - allow: all
  mod_filter_message:
    - allow: all
  mod_filter_iq:
    - allow: all
  ## Admins can send anything.  Others are restricted in various ways.
  mod_filter:
    - allow: admin
    - restrict_local: local
    - restrict_foreign: all
  ## Local non-admin users can only send messages to other local users.
  restrict_local:
    - allow: local
    - deny: all
  ## Foreign users can only send messages to admins.
  restrict_foreign:
    - allow: admin
    - deny: all


	EXAMPLE 2
	=========

On this example, the users of a private vhost (example3.org) can only chat with themselves,
so that particular vhost will have no connection to the exterior. The other vhosts on the
server are completely unrestricted. The administrators are also unrestricted.

## This ejabberd server has three virtual hosts
hosts:
  - "localhost"
  - "example1.org"
  - "example2.org"
  - "example3.org"

## This ACL will match any user or service (MUC, PubSub...) hosted on example3.org
acl:
  ex3server:
    server_glob:
      - "*example3.org"

access_rules:
  mod_filter_presence:
    - allow: all
  mod_filter_message:
    - allow: all
  mod_filter_iq:
    - allow: all
  ## The main mod_filter rule allows any admin, but restricts example3 and the rest of packets
  mod_filter:
    - allow: admin
    - restrict_ex3: ex3server
    - restrict_nonex3: all
  ## This rule, which applies to packets sent from Ex3 non-admin users,
  ## allows packets sent to Ex3 server (packets internal to the vhost) and denies anything else.
  restrict_ex3:
    - allow: ex3server
    - deny: all
  ## This rule, which applies to the rest of packets (the ones that are not sent from Ex3),
  ## allows all packets to admins (allowing replies to stanzas from Ex3 admins),
  ## denies all other access to Ex3, and allows access to anything else.
  restrict_nonex3:
    - allow: admin
    - deny: ex3server
    - allow: all


	EXAMPLE 4
	=========


This server has two virtual hosts, one with anonymous users. The anonymous users
cannot send or receive presence stanzas from outside their vhost.

hosts:
  - "localhost"
  - "anon.localhost.org"

acl:
  anon_user:
    server_glob:
      - "*anon.localhost"

access_rules:
  mod_filter:
    - allow: all
  mod_filter_presence:
    - allow: admin
    - restrict_anon: anon_user
    - restrict_non_anon: all
  restrict_anon:
    - allow: anon_user
    - deny: all
  restrict_non_anon:
    - allow: admin
    - deny: anon_user
    - allow: all
  mod_filter_message:
    - allow: all
  mod_filter_iq:
    - allow: all

