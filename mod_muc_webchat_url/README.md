mod_muc_webchat_url - Show webchat_url in MUC rooms disco#info
==============================================================

Requires:
- ejabberd 25.xx


This small module shows webchat_url in MUC rooms disco#info.


Options
-------

This module supports those configurable options:

* `base_url: auto | binary()`

  The URL that will be shown in room disco#info.

  If set to `auto`, it searches the ejabberd configuration for mod_conversejs and generates an URL.
  If set manually, it can use all the keywords available in ejabberd,
  and also those specific keywords: `ROOM_JID`, `ROOM_NAME`, `ROOM_HOST`.

  Default value: `auto`

* `room_names: [binary()]`

  The URL will be shown also to the rooms which name is in this list.

  Default value: `[]`

* `room_options: {OptionName: boolean()}`

  Map of room options that should match in order to show the URL:
  all the rooms that are configured like this will show the URL.

  Default value:
  ```yaml
  room_options:
    members_only: false
    password_protected: false
    public: true
  ```


Configuration with mod_conversejs
---------------------------------

```yaml
listen:
  -
    port: 5443
    module: ejabberd_http
    tls: true
    request_handlers:
      /conversejs: mod_conversejs
      /ws: ejabberd_http_ws

auth_method: anonymous
anonymous_protocol: login_anon

modules:
  mod_conversejs:
    conversejs_resources: "/home/ejabberd/conversejs/10.1.8/dist"
  mod_host_meta: {}
  mod_muc_webchat_url: {}
```

The URLs are like:
`https://localhost:5443/conversejs/#converse/room?jid=room1@conference.localhost`


Example Configuration
---------------------

Example of module configuration:

```yaml
modules:
  mod_muc_webchat_url:
    base_url: "https://@HOST@:443/webclient/join?name=@ROOM_NAME@&host=@ROOM_HOST@&jid=@ROOM_JID@"
    room_names:
      - "general"
      - "support"
    room_options:
      members_only: false
      password_protected: false
      public: true
```

In that case, the URL is shown to rooms named `general` and `support`,
and also to rooms that are public, not password protected and not members_only.

The URLs are like:
`"https://localhost:443/webclient/join?name=room1&host=conference.localhost&jid=room1@conference.localhost"`

