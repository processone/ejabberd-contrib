mod_statsdx - Calculates and gathers statistics actively
========================================================

* Requires: ejabberd 19.08 or higher
* Homepage: http://www.ejabberd.im/mod_statsdx
* Author: Badlop


Measures several statistics. It provides a new section in ejabberd
Web Admin and two ejabberd commands to view the information.


Configuration
-------------

Configurable options:

- `hooks`

  Set to `true` to enable hooks and related statistics.
  This option by default `false` because it is expected to
  consume many resources in very populated servers.
  If set to `traffic`, it will also keep counters of traffic stanzas.


Example Configuration
---------------------

To start using the module, simply enable it:
```yaml
modules:
  mod_statsdx: {}
```

To enable the `hooks` option:

```yaml
modules:
  mod_statsdx:
    hooks: true
```


Feature Requests
----------------

 - fix the problem with plain/ssl/tlsusers, it crashes ejabberd
 - traffic: send bytes per second, received bps
 - connections to a transport
 - traffic: send presence per second, received mps
 - Number of SASL c2s connections
 - improve to work in distributed server


mod_stats2file
==============

Generates files with all kind of statistics

This module writes a file with all kind of statistics every few minutes.
Available output formats are html (example),
text file with descriptions and raw text file (for MRTG, RRDTool...).


Configuration
-------------

This module requires `mod_statsdx`.

Configurable options:
- `interval`: Time between updates, in minutes (default: `5`)
- `type`: Type of output. Allowed values: `html`, `txt`, `dat` (default: `html`)
- `basefilename`: Base filename including absolute path (default: `"/tmp/ejasta"`)
- `split`: If split the statistics in several files (default: `false`)
- `hosts`: List of virtual hosts that will be checked. By default `all`


Example Configuration
---------------------

The module can be simply enabled, accepting the default options configuration:
```yaml
modules:
  mod_stats2file: {}
```

Or set some options:
```yaml
modules:
  mod_stats2file:
    interval: 60
    type: txt
    split: true
    basefilename: "/var/www/stats"
    hosts: ["localhost", "server3.com"]
```
