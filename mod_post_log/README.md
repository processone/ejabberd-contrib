# mod_post_log - Logs messages to an HTTP API

*Homepage*: https://github.com/processone/ejabberd-contrib/tree/master/mod_post_log
*Author*: Tim Stewart, Mojo Lingo LLC
*Requirements*: ejabberd 14.07 or later

This module implements logging of all messages sent (chat and groupchat) via an HTTP API.


## CONFIGURATION

Add the module to your ejabberd.yml, in the modules section:

```yaml
modules:
  ...
  mod_post_log:
    url: http://foo.bar.com/messages
  ...
```

## API example

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
