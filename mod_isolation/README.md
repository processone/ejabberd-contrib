mod_isolation: Isolate virtual hosts from each other
====================================================

The module blocks communication of users between different virtual hosts.

# Configuration

The module doesn't have any options, so the simpliest way to configure is:
```yaml
modules:
  mod_isolation: {}
```

Hint: if you also want to block whole s2s, use built-in ejabberd's `s2s_access` option:
```yaml
s2s_access: none

modules:
  mod_isolation: {}
```
