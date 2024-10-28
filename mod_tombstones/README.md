mod_tombstones - Keep graveyard of accounts and rooms tombstones
================================================================

Requires:
- ejabberd 24.10 or higher


Keep tombstones for accounts and rooms that were removed.
This prevents registration of that account, and creation of that room.


Options
-------

The configurable options are:

- `db_type = mnesia | sql | ldap`

  Same as top-level `default_db` option, but applied to this module only.
  Default: `mnesia`

- `room_tombstone_expiry = time | infinity`

  How long to keep MUC room tombstones.
  If set to `infinity` the tombstones are kept forever.
  The default value is `30 days`.

- `user_tombstone_expiry = time | infinity`

  How long to keep users tombstones.
  If set to `infinity` the tombstones are kept forever.
  The default value is `365 days`.


Example Configuration
---------------------

This configuration keeps accounts for one month, and rooms for a week:

```yaml
modules:
  mod_tombstones:
    room_tombstone_expiry: 7 days
    user_tombstone_expiry: 30 days
```


