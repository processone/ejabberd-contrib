
	mod_default_rooms - Add MUC bookmark(s) on registration

	Author: Holger Weiss <holger@zedat.fu-berlin.de>


	DESCRIPTION
	-----------

This module allows for specifying one or more rooms that should be
bookmarked automatically on successful user registration (via
"mod_register", or, for example, "ejabberdctl register").


	CONFIGURATION
	-------------

In order to use this module, add a configuration snippet such as the
following:

  modules:
    # [...]
    mod_default_rooms:
      rooms:
        - "foo@conference.example.net"
        - "bar@conference.example.org"

The configurable mod_default_rooms options are:

- rooms (default: [])

  The list of rooms users that should be auto-bookmarked on account
  registration.

- auto_join (default: true)

  This option specifies wether the auto-join flag should be set for the
  bookmarks created on registration.
