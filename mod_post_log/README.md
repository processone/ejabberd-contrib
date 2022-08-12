mod_post_log - Logs messages to an HTTP API
===========================================

* Author: Tim Stewart, Mojo Lingo LLC
* Requirements: ejabberd 14.07 or later

This module implements logging of all messages sent (chat and groupchat) via an HTTP API.


Configuration
-------------

- `url`:
  URL where the HTTP POSTs are to be sent.

- `ts_header`:
  Default value: `"X-Message-Timestamp"`

- `from_header`:
  Default value: `"X-Message-From"`

- `to_header`:
  Default value: `"X-Message-To"`

- `headers`:
  Default value: `[]`

- `content_type`:
  Default value: `"text/xml"`

- `http_options`:
  Default value: `[]`

- `req_options`:
  Default value: `[]`


Example Configuration
---------------------

```yaml
modules:
  mod_post_log:
    url: "http://foo.bar.com/messages"
```


API Example
-----------

```
POST /messages HTTP/1.0
HTTP-X-MESSAGE-FROM: doo@dah.com
HTTP_X_MESSAGE_TO: foo@bar.com
Content-Type: application/xml
Content-Length: 122

<message to='foo@bar.com' from='doo@dah.com' type='chat'>
  <body xmlns='jabber:client'>Hello there Foo!</body>
</message>
```
