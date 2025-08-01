mod_pubsub_serverinfo - Exposes server information over Pub/Sub
==============================================================

* Author: Stefan Strigler <stefan@strigler.de>

Initially created by Guus der Kinderen.

NOTE: This module was included in ejabberd 25.07, you don't need to install this old version.


Description
-----------

Announces support for the ProtoXEP PubSub Server Information, by adding
its Service Discovery feature.

Active S2S connections are published to a local pubsub node as advertised by
Service Discovery. Only those connections that support this feature as well are
exposed with their domain names, otherwise they are shown as anonymous nodes. At
startup a list of well known public servers is being fetched. Those are not
shown as anonymous even if they don't support this feature.

Currently the name of the node is hardcoded as "serverinfo". The local service
to be used can be configured as `pubsub_host`. Otherwise a good guess is taken.

This module has a hard dependency on `mod_pubsub` for this reason. Also
`mod_disco` must be configured for this feature to work.

> [!NOTE]
> The module only shows S2S connections established while the module is running:
> after installing the module, please run `ejabberdctl stop_s2s_connections`, or
> restart ejabberd.

Configuration
-------------

Configurable options for `mod_pubsub_serverinfo`:

- `pubsub_host` (default: undefined)

  This option specifies which pubsub host to use to advertise S2S connections.
  This must be a vhost local to this service and handled by `mod_pubsub`. This
  is only needed if your configuration has more than one vhost in mod_pubsub's
  `hosts` option. If there's more than one and this option is not given, we just
  pick the first one.
