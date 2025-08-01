mod_spam_filter - Filter spam messages based on JID/content
===========================================================

* Author: Holger Weiss <holger@zedat.fu-berlin.de>
* Author: Stefan Strigler <stefan@strigler.de>

NOTE: This module was improved, renamed to mod_antispam and included in ejabberd 25.07


Description
-----------

This module allows for filtering spam messages and subscription requests
received from remote servers based on RTBL domain lists, lists of known spammer
JIDs and/or URLs mentioned in spam messages. Traffic classified as spam is
rejected with an error (and an `[info]` message is logged) unless the sender is
subscribed to the recipient's presence. An access rule can be specified to
control which recipients are subject to spam filtering.


Configuration
-------------

To enable this module, configure it like this:

```yaml
modules:
  mod_spam_filter:
    rtbl_host: "xmppbl.org"
    spam_jids_file: "/etc/ejabberd/spam-filter/jids.txt"
    spam_urls_file: "/etc/ejabberd/spam-filter/urls.txt"
```

The configurable `mod_spam_filter` options are:

- `rtbl_host` (default: none)

  This option specifies which RTBL host to use to query for blocked domains. One
  such host is located at `xmppbl.org`. Messages and presence-subscribe stanzas
  originating from domains listed at this host will be blocked in case there is
  no active roster subscription of the recipeient. If a dump file is configured,
  messages will get logged. Senders will receive an abuse notification message.
  The command `ejabberdctl get_blocked_domains` retrieves the current list of
  blocked domains.

- `rtbl_domains_node` (default: `spam_source_domains`)

  The name of the RTBL node to query for the list of blocked domains.

- `spam_domains_file` (default: `none`)

  This option specifies the path to a plain text file containing a list of known
  spam domains, one domain per line. Messages and subscription requests sent
  from one of the listed domains will be classified as spam if sender is not in
  recipient's roster. This list of domains will be merged with the one retrieved
  by an RTBL host if any given. The behavior is the same.
  
- `whitelist_domains_file` (default: `none`)

  This option allows you to provide a path to a file containing a list of
  domains to whitelist from being blocked, one per line. If either it is in
  `spam_domains_file` or more realistically in a domain sent by a RTBL host (see
  option `rtbl_host`) then this domain will be ignored and stanzas from there
  won't be blocked.
  
- `spam_dump_file` (default: `none`)

  This option specifies the path to a file that messages classified as
  spam will be written to.  The messages are dumped in raw XML format, and
  a `<delay/>` tag with the current timestamp is added.  The `@HOST@` keyword
  will be substituted with the name of the virtual host.  Note that this
  module doesn't limit the file size, so if you use this option, make sure
  to monitor disk file usage and to rotate the file if necessary.  After
  rotation, the command `ejabberdctl reopen-log` can be called to let the
  module reopen the spam dump file.

- `spam_jids_file` (default: `none`)

  This option specifies the path to a plain text file containing a list of
  known spammer JIDs, one JID per line.  Messages and subscription
  requests sent from one of the listed JIDs will be classified as spam.
  Messages containing at least one of the listed JIDs will be classified
  as spam as well.  Furthermore, the sender's JID will be cached, so that
  future traffic originating from that JID will also be classified as
  spam.

- `spam_urls_file` (default: `none`)

  This option specifies the path to a plain text file containing a list of
  URLs known to be mentioned in spam message bodies.  Messages containing
  at least one of the listed URLs will be classified as spam.
  Furthermore, the sender's JID will be cached, so that future traffic
  originating from that JID will be classified as spam as well.

- `access_spam` (default: `none`)

  This option defines the access rule to control who will be subject to
  spam filtering.  If the rule returns `allow` for a given recipient, spam
  messages aren't rejected for that recipient.  By default, all recipients
  are subject to spam filtering.

- `cache_size` (default: `10000`)

  This option specifies the maximum number of JIDs that will be cached due
  to sending spam URLs (see above).  If that limit is exceeded, the least
  recently used entries are removed from the cache.  Setting this option
  to `0` disables the caching feature.  Note that separate caches are used
  for each virtual host, and that the caches aren't distributed across
  cluster nodes.


ejabberd Commands
-----------------

This module provides ejabberdctl/API calls to reread the spam JID/URL
files, to print the JID cache contents, and to add or remove entries from that
cache.  

Furthermore you can add/remove items to/from and query the list of blocked
domains as retrieved from the RTBL host.

See:

```
$ ejabberdctl help add_blocked_domain
$ ejabberdctl help add_to_spam_filter_cache
$ ejabberdctl help drop-from-spam-filter-cache
$ ejabberdctl help expire-spam-filter-cache
$ ejabberdctl help get-spam-filter-cache
$ ejabberdctl help get_blocked_domains
$ ejabberdctl help reload-spam-filter-files
$ ejabberdctl help remove_blocked_domain
```
