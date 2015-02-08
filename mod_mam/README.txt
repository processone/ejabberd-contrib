
	mod_mam - Message Archive Management (XEP-0313)

	Author: Holger Weiss <holger@zedat.fu-berlin.de>
	Requirements: ejabberd 14.12 or newer


	DESCRIPTION
	-----------

This module implements XEP-0313: Message Archive Management (MAM).  MAM
provides server-side storage of messages, which allows for proper
synchronization of multiple clients.


	CONFIGURATION
	-------------

In order to use this module with the default settings, add the following
line to the 'modules' section of your ejabberd.yml file:

  mod_mam: {}

The configurable options are:

- access_max_user_messages (default: 'max_user_mam_messages')

  This option defines which access rule is used to limit the maximum number
  of MAM messages a user can have.  That rule should either yield 0, which
  disables MAM storage for the corresponding user(s), or a positive number,
  or 'infinity' (which is the default).  If the limit is exceeded, the
  oldest MAM message is (silently) discarded when a new one is stored.

- default_page_size (default: 25)

  If a large set of messages is requested using MAM, the response is split
  into smaller chunks ("pages").  The client can optionally specify the
  number of messages it would like to receive per page.  This option
  specifies the number of messages returned if the client doesn't do that.

- max_page_size (default: 100)

  This option limits the number of messages transmitted per chunk.  If the
  client requests larger pages, 'max_page_size' is used instead.

- request_activates_archiving (default: 'true')

  By default, message archiving for a given user won't be enabled before one
  of his clients issued a MAM request.  If message archiving should instead
  be activated for all users immediately, set 'request_activates_archiving'
  to 'false'.

- iqdisc (default: 'parallel')

  The module also supports the 'iqdisc' option, as documented here:

  https://www.process-one.net/docs/ejabberd/guide_en.html#modiqdiscoption
