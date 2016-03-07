mod_mam_mnesia
==============

Current Status
--------------

_This module is deprecated._

ejabberd 15.06 and newer ships a `mod_mam` module which supports Mnesia
and SQL/ODBC storage.  It is recommended to use that module, as
`mod_mam_mnesia` is no longer supported.  Existing `mod_mam_mnesia`
tables can be imported into `mod_mam` using the `import-mam-archives`
script, as described below.

Migrating to mod_mam
--------------------

You might want to log off your users during the migration.  However,
note that importing the MAM archives might take up to a few hours.

Also note that with `mod_mam`, the total size of all MAM archives cannot
exceed 2 GB.  The `delete_old_mam_messages` command could be run
periodically to make sure the Mnesia data won't grow beyond that limit.
To support larger archives, SQL/ODBC storage must be used.

1. In your ejabberd.yml file, replace `mod_mam_mnesia` with `mod_mam`,
   and adjust the configuration of that module.  `mod_mam` supports a
   different set of options, so you should check [the documentation][1].
   Since ejabberd 16.02, `mod_mam` supports the
   `request_activates_archiving` option, but it's not enabled by
   default.  To mimic `mod_mam_mnesia`'s default behavior, you could
   configure `mod_mam` like this:

        modules:
          mod_mam:
            default: always
            request_activates_archiving: true

2. Check the node name of your server by running `ejabberdctl status`.
   If the name is _not_ `ejabberd@localhost`, you must replace the
   `localhost` part of the two node names at the top of the script
   with the host name part of your ejabberd node.

3. The `import-mam-archives` script _removes_ the `mod_mam_mnesia`
   tables after importing them into `mod_mam`.  Therefore, **you should
   take a backup of your MAM archives** (and the other Mnesia data) by
   running a command such as:

        ejabberdctl dump $HOME/ejabberd-backup.dat

4. [Download][2] and run the import script:

        chmod +x import-mam-archives
        ./import-mam-archives

   If only the messages stored during the last `N` days should be
   imported, run `./import-mam-archives N` instead.

5. Restart ejabberd.

[1]: https://docs.ejabberd.im/admin/guide/configuration/#modmam
[2]: https://raw.githubusercontent.com/processone/ejabberd-contrib/master/mod_mam_mnesia/import-mam-archives
