mod_grafite - Gathers statistics and publishes via statsd/grafite
=================================================================

* Author: Thiago Rocha Camargo (rochacamargothiago@gmail.com)

Gathers statistics and publishes via statsd/grafite


Configuration
-------------

Configurable options:
- `statsdhost`: Host of the statsd server
- `statsdport`: Port of the statsd server


Example Configuration
---------------------

```yaml
modules:
  mod_grafite:
    statsdhost: "carbonr.xmpp.com.br"
    statsdport: 8125
```


Feature Requests
---------------

 - Add support for configurable Hooks
