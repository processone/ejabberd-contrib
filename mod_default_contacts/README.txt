
	mod_default_contacts - Add roster contact(s) on registration

	Author: Holger Weiss <holger@zedat.fu-berlin.de>


	DESCRIPTION
	-----------

This module allows for specifying one or more contacts that should be
added to the user's roster automatically on successful registration (via
"mod_register", or, for example, "ejabberdctl register").  Note that no
presence subscription is performed, and the rosters of the contacts aren't
modified.


	CONFIGURATION
	-------------

In order to use this module, add a configuration snippet such as the
following:

  modules:
    # [...]
    mod_default_contacts:
      contacts:
        -
          name: "Bob Virding"
          jid: "bob@example.com"
        -
          name: "Alice Armstrong"
          jid: "alice@example.com"

The configurable mod_default_contacts options are:

- contacts (default: [])

  The list of contact JIDs that should be auto-added to the user's roster
  on account registration.  Each list item must specify the 'jid:' and
  optionally the 'name:' of the contact.
