mod_pubsub_serverinfo - Exposes server information over Pub/Sub
==============================================================

* Author: Guus der Kinderen <guus.der.kinderen@gmail.com>

Description
-----------

Announces support for the ProtoXEP PubSub Server Information, by adding
its Service Discovery feature.

Future versions of this plugin are expected to publish data describing
the local XMPP domain(s).

> [!NOTE]
> The module only shows S2S connections established while the module is running:
> after installing the module, please run `ejabberdctl stop_s2s_connections`, or
> restart ejabberd.

Configuration
-------------

This module does not have any configurable settings for now.

> [!NOTE] 
> As of now your pubsub component must be reachable at `pubsub.<yourhost>` (the
> default), otherwise this module would advertise the wrong address.
> See: https://github.com/processone/ejabberd-contrib/issues/343
