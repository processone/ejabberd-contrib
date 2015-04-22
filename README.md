ejabberd-contrib
================

This is a collaborative development area for ejabberd module developers
and users.


For users
---------

To use an ejabberd module coming from this repository:

- You need to have ejabberd installed.

- Run `ejabberdctl module_install <module>` to get the source code and to
  compile and install the `beam` file into ejabberd's module search path.
  This path is either `~/.ejabberd-modules` or defined by the
  `CONTRIB_MODULES_PATH` setting in `ejabberdctl.cfg`.

- Edit the configuration file provided in the `conf` directory of the
  installed module and update it to your needs. Then apply the changes to
  your main ejabberd configuration. In a future release, ejabberd will
  automatically add this file to its runtime configuration without
  changes.

- Run `ejabberdctl module_uninstall <module>` to remove a module from
  ejabberd.


For developers
--------------

The following organization has been set up for the development:

- Development and compilation of modules is done by ejabberd. You need
  ejabberd installed. Use `ejabberdctl module_check <module>` to ensure it
  compiles correctly before committing your work. The sources of your
  module must be located in `$CONTRIB_MODULES_PATH/sources/<module>`.

- Compilation can by done manually (if you know what you are doing) so you
  don't need ejabberd running:
  ```
  cd /path/of/module
  mkdir ebin
  /path/of/ejabberd's/erlc \
     -o ebin \
     -I include -I /path/of/ejabberd/lib/ejabberd-XX.YY/include \
     -DLAGER -DNO_EXT_LIB \
     src/*erl
  ```

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
