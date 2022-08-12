mod_webadmin_config: Show Configuration in Web Admin
====================================================

This module shows a page in ejabberd's Web Admin to view and reload the configuration.

The page is available in Web Admin -> Nodes -> your node.

It shows the node's current configuration as stored in the internal memory,
which includes the content of the main `ejabberd.yml`,
the content of any files added with `include_config_file`,
and the configuration added by contributed modules.

The page also allows to reload the configuration,
similarly to the `reload_config` API command.
It also displays a diff of last changes detected in the configuration.

# Configuration

The module has no options to configure, it's simply enabled with:
```yaml
modules:
  mod_webadmin_config: {}
```
