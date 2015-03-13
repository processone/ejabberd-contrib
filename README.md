ejabberd-contrib
================

This is a collaborative development area for ejabberd module developers
and users.


For users
---------

To use an ejabberd module coming from this repository:

- You need to have Ejabberd installed.

- Run `ejabberdctl module_install <module>` to get the sources, compile and
  install the beams file in ejabberd path. This path is either ~/.ejabberd-modules
  or defined by CONTRIB\_MODULES\_PATH in ejabberdctl.cfg.

- Edit the configuration file provided in the `conf` dir of the installed
  module to update to your needs. Then apply the changes to your main ejabberd
  configuration. On future release, ejabberd will automatically add this file
  to its runtime configuration without changes.

- Run `ejabberdctl module_uninstall <module>` to remove a module from ejabberd.

For developers
--------------

The following organization has been set up for the development:

- Development and compilation of modules is done by ejabberd. You need
  Ejabberd installed. Use `ejabberdctl module_check <module>` to ensure
  it compiles correctly before commit your work. Sources of your module
  must be located in $CONTRIB\_MODULES\_PATH/sources/<module>

- The module directory structure is usually the following:
    * `README.txt`: Module description.
    * `COPYING`: License for the module.
    * `doc/`: Documentation directory.
    * `src/`: Erlang source directory.
    * `lib/`: Elixir source directory.
    * `priv/msgs/`: Directory with translation files (pot, po and msg).
    * `conf/<module>.yml`: Configuration for your module.
    * `<module>.spec`: Yaml description file for your module.

- Module developers should note in the `README.txt` file whether the
  module has requirements or known incompatibilities with other modules.
