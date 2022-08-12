mod_muc_log_http - Serve MUC logs on the web
============================================

* Requires: ejabberd 19.08 or higher
* Homepage: http://ejabberd.im/mod_muc_log_http
* Author: Badlop


Description
-----------

This module serves the directory containing MUC logs already
configured on `mod_muc_log`.  So, there's no need to setup a web server
to allow your users to view the MUC logs.  It is a small modification
of `mod_http_fileserver`, customized for log serving.


Configuration
-------------

This module has no configurable options, simply enable it.


Example Configuration
---------------------

The directory to serve is already defined on `mod_muc_log`.

```yaml
listen:
  -
    port: 5282
    module: ejabberd_http
    request_handlers:
      /muclogs: mod_muc_log_http

modules:
  mod_muc_log:
    outdir: "/tmp/muclogs"
  mod_muc_log_http: {}
```

With that example configuration, open your web browser at:
`http://server:5282/muclogs/`
