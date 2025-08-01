# mod_unified_push - Conversations-compatible UnifiedPush push server implementation

- Author: itd (https://codeberg.org/itd)
- Copyright (C) 2025 itd

This module adds Unified Push support compatible with Conversations.
When the module is enabled, the server can be added as a Unified Push distributor
in the settings of Conversations, using the same domain used for the JID.

ejabberd newer than 25.04 is required,
as it requires an updated version of the
[xmpp erlang library](https://github.com/processone/xmpp)
that includes the improvements from the file `xmpp_spec.patch`.
Or you can
[apply the patch yourself](https://github.com/processone/xmpp/issues/9#issue-205855303)


## Configuration

When you install this module, it's enabled by default on port 5283, see `conf/mod_unified_push.yml`

If you want to customize that configuration,
remove all lines in the file `mod_unified_push.yml` (don't remove the file itself),
and now you can configure the module in your main `ejabberd.yml` configuration file,
for example:

```
listen:
  -
    port: 5443
    ip: "::"
    module: ejabberd_http
    tls: true
    request_handlers:
      /api: mod_http_api
      /bosh: mod_bosh
      /up: mod_unified_push
      /upload: mod_http_upload
      /ws: ejabberd_http_ws

modules:
  mod_unified_push: {}
  ...
```
