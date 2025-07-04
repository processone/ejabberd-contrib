# mod_unified_push - Conversations-compatible UnifiedPush push server implementation

- Author: itd (https://codeberg.org/itd)
- Copyright (C) 2025 itd

This module adds Unified Push support compatible with Conversations.

ejabberd newer than 25.04 is required,
as it requires an updated version of the
[xmpp erlang library](https://github.com/processone/xmpp)
that includes the improvements from the file `xmpp_spec.patch`.
Or you can
[apply the patch yourself](https://github.com/processone/xmpp/issues/9#issue-205855303)

## Configuration

To use this module, load `mod_unified_push` in the `modules` section of the config file and add `/up: mod_unified_push` to a listen directive:

```
  -
    port: 5443
    ip: "::"
    module: ejabberd_http
    tls: true
    protocol_options: 'TLS_OPTIONS'
    request_handlers:
      /api: mod_http_api
      /bosh: mod_bosh
      /upload: mod_http_upload
      /ws: ejabberd_http_ws
      /up: mod_unified_push
```

Once the config has been reloaded, the server can be added as a Unified Push distributor in the settings of Conversations, using the same domain used for the JID.
