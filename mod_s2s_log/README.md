mod_s2s_log - Log all s2s connections in a file
===============================================


This module can be used to keep track of other XMPP servers your server has
been connected with.

Example configuration:
```yaml
modules:
  mod_s2s_log:
    filename: "/path/to/s2s.log"
```
