mod_logxml - Log XMPP packets to XML file
=========================================

* Homepage: http://www.ejabberd.im/mod_logxml
* Author: Badlop
* Requires: ejabberd 19.08 or higher


Description
-----------

This module sniffs all the XMPP traffic send and received by ejabberd,
both internally and externally transmitted. It logs the XMPP packets
to a XML formatted file. It's posible to filter transmitted packets
by orientation, stanza and direction. It's possible to configure the
file rotation rules and intervals.

This module reuses code from `mod_log_forensic`, `mod_stats2file`, `mod_muc_log`.


Configuration
-------------

- `stanza`:
    Log packets only when stanza matches.
    Default value: `[iq, message, presence, other]`
- `direction`:
    Log packets only when direction matches.
    Default value: `[internal, vhosts, external]`
- `orientation`:
    Log packets only when orientation matches.
    Default value: `[send, recv]`
- `logdir`:
    Base filename, including absolute path.
    If set to `auto`, it uses the ejabberd log path.
    Default value: `auto`
- `show_ip`:
    If the IP address of the local user should be logged to file.
    Default value: `false`
- `rotate_days`:
    Rotate logs every X days.
    Put 0 to disable this limit.
    Default value: `1`
- `rotate_megs`:
    Rotate when the logfile size is higher than this, in megabytes.
    Put 0 to disable this limit.
    Default value: `10`
- `rotate_kpackets`:
    Rotate every *1000 XMPP packets logged.
    Put `0` to disable this limit.
    Default value: `10`
- `check_rotate_kpackets`:
    Check rotation every `*1000` packets.
    Default value: `1`


Example Configuration
---------------------

```yaml
modules:
  mod_logxml:
    stanza:
      - iq
      - other
    direction:
      - external
    orientation:
      - send
      - recv
    logdir: "/tmp/logs/"
    show_ip: false
    rotate_days: 1
    rotate_megs: 100
    rotate_kpackets: 0
    check_rotate_kpackets: 1
```

XML Format
----------

XMPP packets are enclosed in `<packet>`, with attributes:
- `or`: orientation of the packet, either `send` or `recv`
- `ljid`: local JID of the sender or receiver, depending on the orientation
- `ts`: timestamp when the packet was logged
