mod_http_redirect - Redirect HTTP path to another URI
============================================================

* Author: Badlop


Description
-----------


This simple module redirects the web browser to the configured URI.


Configuration
-------------

The configurable option is:

- `location` (default: `[]`)

  The URI where requests will be redirected.

This module should be added not only to the `modules` section,
also to `request_handlers` inside the `listen` option.

It is very important to set this request handler as the last one,
otherwise it could produce infinite redirects.

```yaml
listen:
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /admin: ejabberd_admin
      /register: mod_register_web
      /: mod_http_redirect

modules:
  mod_http_redirect:
    location: http://example.com
```

With that configuration, a request for `http://localhost:5280/`
will be redirected to `http://example.com`

