
	mod_spam_filter - Filter spam messages based on JID/content

	Author: Holger Weiss <holger@zedat.fu-berlin.de>


	DESCRIPTION
	-----------

This module allows for filtering spam messages and subscription requests
received from remote servers based on lists of known spammer JIDs and/or
URLs mentioned in spam messages.  Traffic classified as spam is rejected
with an error (and an [info] message is logged) unless the sender is
subscribed to the recipient's presence.  An access rule can be specified
to control which recipients are subject to spam filtering.


	CONFIGURATION
	-------------

In order to use this module, add a configuration snippet such as the
following:

  modules:
    # [...]
    mod_spam_filter:
      spam_jids_file: "/etc/ejabberd/spam-filter/jids.txt"
      spam_urls_file: "/etc/ejabberd/spam-filter/urls.txt"

The configurable mod_spam_filter options are:

- spam_dump_file (default: none)

  This option specifies the path to a file that messages classified as
  spam will be written to.  The messages are dumped in raw XML format, and
  a <delay/> tag with the current timestamp is added.  The @HOST@ keyword
  will be substituted with the name of the virtual host.  Note that this
  module doesn't limit the file size, so if you use this option, make sure
  to monitor disk file usage and to rotate the file if necessary.  After
  rotation, the command "ejabberdctl reopen-log" can be called to let the
  module reopen the spam dump file.

- spam_jids_file (default: none)

  This option specifies the path to a plain text file containing a list of
  known spammer JIDs, one JID per line.  Messages and subscription
  requests sent from one of the listed JIDs will be classified as spam.
  Messages containing at least one of the listed JIDs will be classified
  as spam as well.  Furthermore, the sender's JID will be cached, so that
  future traffic originating from that JID will also be classified as
  spam.

- spam_urls_file (default: none)

  This option specifies the path to a plain text file containing a list of
  URLs known to be mentioned in spam message bodies.  Messages containing
  at least one of the listed URLs will be classified as spam.
  Furthermore, the sender's JID will be cached, so that future traffic
  originating from that JID will be classified as spam as well.

- access_spam (default: none)

  This option defines the access rule to control who will be subject to
  spam filtering.  If the rule returns 'allow' for a given recipient, spam
  messages aren't rejected for that recipient.  By default, all recipients
  are subject to spam filtering.

- cache_size (default: 10000)

  This option specifies the maximum number of JIDs that will be cached due
  to sending spam URLs (see above).  If that limit is exceeded, the least
  recently used entries are removed from the cache.  Setting this option
  to 0 disables the caching feature.  Note that separate caches are used
  for each virtual host, and that the caches aren't distributed across
  cluster nodes.


	ejabberd COMMANDS
	-----------------

This module provides ejabberdctl/API calls to reread the spam JID/URL
files, to print the JID cache contents, and to remove entries from that
cache.  See:

$ ejabberdctl help reload-spam-filter-files
$ ejabberdctl help get-spam-filter-cache
$ ejabberdctl help expire-spam-filter-cache
$ ejabberdctl help drop-from-spam-filter-cache
