mod_message_log - Log one line per message transmission
=======================================================

* Author: Holger Weiss <holger@zedat.fu-berlin.de>


Description
-----------

This module writes a line for each sent or received message to a log file.
Each line mentions the sender's JID and the recipient's JID, and also the
message type (e.g., `"normal"`, `"chat"`, or `"groupchat"`).  Carbon copies are
marked as such.  The log lines look similar to this one:

```
2014-05-25 11:55:04 [outgoing, normal] dan@example.com/Foo -> eve@example.net/Bar
```

After log rotation, you can execute the following command in order to tell
`mod_message_log` to reopen the log file:

```
ejabberdctl reopen_log
```


Configuration
-------------

- `filename`

  Define the filename and path to store the log.
  If the filename option is set to `auto`, it will be set to
  the default ejabberd log path, with the file name `"message.log"`.
  The filename option takes as default value `auto`.


Example Configuration
---------------------

```yaml
modules:
  mod_message_log:
    filename: "/path/to/ejabberd-message.log"
```
