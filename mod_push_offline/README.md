mod_push_offline - Send offline messages to a component
=======================================================

* Requires: ejabberd 22.05 or higher
* Author: Mujtaba Roohani <mujtaba.roohani@gmail.com>
* Copyright (C) 2022 Mujtaba Roohani


This is an ejabberd module that sends all messages sent to an offline entity to the specified [component](https://xmpp.org/extensions/xep-0114.html). It is a small modification of `mod_push`, customized for development of advanced push notification services.

This module can be used as a substitute to [mod_http_offline](https://github.com/raelmax/mod_http_offline/tree/master/) and mod_push if one wants to receive the complete stanza sent to offline entity instead of only the sender and body of the message.

Configuration
-------------

Configurable options:

- `host`: This option defines the host to receive offline messages from the service. If the 'host' option is not specified, the host will be the hostname of the virtual host with the prefix "push.".


Example Configuration
---------------------

You can modify the default module configuration file like this:

To enable the module:
```yaml
modules:
  mod_push_offline:
    host: "push.localhost"
```

To enable the HTTP request handler in the listen section:
```yaml
listen:
  -
    port: 5347
    module: ejabberd_service
    access: all
    shaper_rule: fast
    ip: "::"
    hosts:
      "push.localhost":
        password: "Secret"
```

With that configuration, you can connect a [component](https://xmpp.org/extensions/xep-0114.html) to ejabberd with following credentials to receive offline messages:
 - domain: `push.localhost`
 - password: `Secret`


Example Flow
-----------------

When juliet sends a message to romeo who is unavailable:
```
<message to="romeo@localhost" from="juliet@localhost">
	<body>Wherefore art thou, Romeo?</body>
</message>
```

ejabberd.log shows those messages:
```
2022-10-10 12:40:25.261336+05:00 [debug] <0.559.0>@ejabberd_hooks:safe_apply/4:315 Running hook offline_message_hook: mod_push_modified:offline_message/1
2022-10-10 12:40:25.261496+05:00 [debug] <0.559.0>@mod_push_modified:offline_message/1:79 Notifying romeo@localhost of offline message
2022-10-10 12:40:25.263651+05:00 [debug] <0.559.0>@ejabberd_router:do_route/1:384 Route:
#message{
 id = <<"9202008986926435556">>,type = normal,lang = <<>>,
 from =
  #jid{
   user = <<"romeo">>,server = <<"localhost">>,resource = <<>>,
   luser = <<"romeo">>,lserver = <<"localhost">>,
   lresource = <<>>},
 to =
  #jid{
   user = <<>>,server = <<"push.localhost">>,resource = <<>>,
   luser = <<>>,lserver = <<"push.localhost">>,lresource = <<>>},
 subject = [],body = [],thread = undefined,
 sub_els =
  [#ps_event{
    items =
     #ps_items{
      xmlns = <<>>,node = <<"urn:xmpp:push:nodes:messages">>,
      items =
       [#ps_item{
         xmlns = <<>>,id = <<"9202008986926435556">>,
         sub_els =
          [#message{
            id = <<"98b12ff6-f196-487d-99f3-08c9406474db">>,type = chat,
            lang = <<"en">>,
            from =
             #jid{
              user = <<"juliet">>,server = <<"localhost">>,
              resource = <<"balcony">>,luser = <<"juliet">>,
              lserver = <<"localhost">>,lresource = <<"balcony">>},
            to =
             #jid{
              user = <<"romeo">>,server = <<"localhost">>,
              resource = <<>>,luser = <<"romeo">>,
              lserver = <<"localhost">>,lresource = <<>>},
            subject = [],
            body = [#text{lang = <<>>,data = <<"Wherefore art thou, Romeo?">>}],
            thread = undefined,
            sub_els = [],
            meta = #{ip => {0,0,0,0,0,0,0,1}}}],
         node = <<>>,publisher = <<>>}],
      max_items = undefined,subid = <<>>,retract = undefined},
    purge = undefined,subscription = undefined,delete = undefined,
    create = undefined,configuration = undefined}],
 meta = #{}}
2022-10-10 12:40:25.263991+05:00 [notice] <0.552.0> (tcp|<0.552.0>) Send XML on stream = <<"<message to='push.localhost' from='juliet@localhost' id='9202008986926435556'><event xmlns='http://jabber.org/protocol/pubsub#event'><items node='urn:xmpp:push:nodes:messages'><item id='9202008986926435556'><message to="romeo@localhost" from="juliet@localhost"><body>Wherefore art thou, Romeo?</body></message></item></items></event></message>">>
```

If the component push.localhost is connected, it will receive this message:
```
<message to='push.localhost' from='juliet@localhost' id='9202008986926435556'>
	<event
		xmlns='http://jabber.org/protocol/pubsub#event'>
		<items node='urn:xmpp:push:nodes:messages'>
			<item id='9202008986926435556'>
				<message to="romeo@localhost" from="juliet@localhost">
					<body>Wherefore art thou, Romeo?</body>
				</message>
			</item>
		</items>
	</event>
</message>
```
