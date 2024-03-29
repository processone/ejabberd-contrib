mod_filter - Flexible Filtering by Server Policy
================================================

* Author: Magnus Henoch <henoch@dtek.chalmers.se>
* Copyright (C) 2005 Magnus Henoch


This module allows the admin to specify packet filtering rules using ACL and ACCESS.


ejabberd Patch
--------------

Since ejabberd 19.08, it is necessary to apply a small patch to ejabberd
source code in order to use complex `access_rules` configurations, like the
ones shown in examples 1, 2, 3, 4...

So, apply this patch your ejabberd source code.
As you can see, it only adds a line.
Then recompile ejabberd, reinstall and restart it:

```diff
diff --git a/src/acl.erl b/src/acl.erl
index d13c05601..c2a72fd9f 100644
--- a/src/acl.erl
+++ b/src/acl.erl
@@ -310,6 +310,7 @@ access_rules_validator() ->
       econf:non_empty(
 	econf:options(
 	  #{allow => access_validator(),
+	    '_' => access_validator(),
 	    deny => access_validator()},
 	  []))).
 
--
```


Configuration
-------------

You can modify the default module configuration file like this:

To enable the module:
```yaml
modules:
  mod_filter: {}
```

And you must also add the default access rules:
```yaml
access_rules:
  mod_filter:
    - allow: all
  mod_filter_presence:
    - allow: all
  mod_filter_message:
    - allow: all
  mod_filter_iq:
    - allow: all
```

The configuration of rules is done using ejabberd's ACL and ACCESS,
so you should also study the corresponding section on ejabberd guide.
You can find here several examples that may help you to understand how it works.


Example 1
---------

```yaml
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
```


Example 2
---------

On this example, the users of a private vhost (`example3.org`) can only chat with themselves,
so that particular vhost will have no connection to the exterior. The other vhosts on the
server are completely unrestricted. The administrators are also unrestricted.

```yaml
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
```


Example 4
---------

This server has two virtual hosts, one with anonymous users. The anonymous users
cannot send or receive presence stanzas from outside their vhost.

```yaml
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
```
