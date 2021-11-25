ejabberd-contrib
================

This is a collaborative development area for ejabberd module developers
and users.

Those modules are not officially supported by ProcessOne.

For users
---------

To use an ejabberd module coming from this repository:

- You need to have ejabberd installed.
 
- If you have not already done it, run `ejabberdctl modules_update_specs`
  to retrieve the list of available modules.

- Run `ejabberdctl module_install <module>` to get the source code and to
  compile and install the `beam` file into ejabberd's module search path.
  This path is either `~/.ejabberd-modules` or defined by the
  `CONTRIB_MODULES_PATH` setting in `ejabberdctl.cfg`.

- Edit the configuration file provided in the `conf` directory of the
  installed module and update it to your needs. Or, if you prefer so,
  configure it in your main ejabberd configuration file.

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

- If your module project contains several erlang modules, you should export a
  function pre_uninstall/0 in the main one listing the other ones.
  See mod_statsdx as an example.


Broken modules
--------------

This is the list of modules that are known to be broken with latest ejabberd master branch.

If you feel they are worth it, your help to fix them is welcome:

 - atom_pubsub: "Provides access to all PEP nodes via an AtomPub interface."
 - ircd: "This is an IRC server frontend to ejabberd."
 - mod_archive: "Message Archiving (XEP-0136)."
 - mod_irc: "IRC transport."
 - mod_mam_mnesia: This feature got included in ejabberd 15.06
 - mod_openid: "Transform the Jabber Server in an openid provider."
 - mod_profile: "User Profile (XEP-0154) in Mnesia table."
