
	mod_deny_omemo - Prevent OMEMO sessions from being established

	Author: Holger Weiss <holger@zedat.fu-berlin.de>


	DESCRIPTION
	-----------

Unless the configured access rule (called 'omemo' by default) returns
'allow', this module prevents OMEMO sessions from being established.
Requests to query the devicelist from other users are rejected.  Requests
to publish a devicelist are also rejected, and all OMEMO nodes referenced
in that devicelist are removed.  Incoming devicelist updates are silently
dropped.


	CONFIGURATION
	-------------

In order to use this module, add configuration snippets such as the
following:

  access_rules:
    # [...]
    omemo:
      - deny:
        - user: "alice@example.com"
        - user: "bob@example.com"
      - allow # Permit OMEMO except for the JIDs above.

  modules:
    # [...]
    mod_deny_omemo: {}

The configurable mod_deny_omemo options are:

- access (default: 'omemo')

  This option defines the access rule to control who will be able to
  establish OMEMO sessions.  The default value is 'omemo'.  Establishing
  OMEMO sessions is only permitted if an access rule of that name exists
  and returns 'allow'.
