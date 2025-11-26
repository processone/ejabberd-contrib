mod_autoreload – Automatic reload on configuration and certificate file changes
================================================
* Requires: ejabberd **25.03** or higher  
* Author: Blieb

Description
-----------
This module monitors SSL certificate files and ejabberd configuration files.  
When any watched file changes, ejabberd automatically reloads its configuration.  
For example when an external process updates certificates or when running inside a Kubernetes cluster.



Configuration
-----------
Add to the `modules` section:

```yaml
modules:
  mod_autoreload:
    watch_ssl: true
    watch_config: false
```

## Options

### `watch_ssl: boolean()`
Monitor the SSL certificate files defined in `certfiles:`.  
If a certificate file is modified, ejabberd reloads its configuration.

Default: **true**

-----------

### `watch_config: boolean()`
Monitor ejabberd's configuration files, including:

- the main `ejabberd.yml`
- files from `include_config_file:`
- external module config files

If any of these change, ejabberd reloads its configuration.

Default: **false**
