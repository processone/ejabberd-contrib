mod_cron - Execute scheduled tasks
==================================

* Requires: ejabberd 19.08 or higher
* http://www.ejabberd.im/mod_cron
* Author: Badlop


This module allows advanced ejabberd administrators to schedule tasks for
periodic and automatic execution. This module is a similar concept than the
Unix's cron program.

Each time a scheduled task finishes its execution, a message is printed in the
ejabberd log file.


Basic Configuration
-------------------

Add the module to the `modules` section on the configuration file:
```yaml
modules:
  mod_cron: {}
```

Then, using the `tasks` option, you can add a list of tasks.
For each task there are options to define _when_ to run it,
and options to define _what_ to run.

When
----

Those options determine when a task is ran:

* `time: integer()`

* `units: seconds | minutes | hours | days`

  Indicates the time unit to use.

* `timer_type: interval | fixed`

  Default value is `interval`.
  Fixed timers occur at a fixed time
  after the [minute|hour|day] e.g. every hour on the 5th minute (1:05PM, 2:05PM etc).
  Interval timers occur every interval (starting on an even unit) e.g. every 10 minutes
  starting at 1PM, 1:10PM, 1:20PM etc.
  Fixed timers are the equivalent of unix cron's comma syntax e.g. `"2 * * *"`
  and interval timers are the `/` syntax e.g. `"*/5 * * *"`.

What
----

You can define a task to run some ejabberd API (either in command or in ctl syntax),
or any arbitrary erlang function.

### Command

Use the option `command` and provide `arguments` in the correct format:

```yaml
command: delete_old_mam_messages
arguments:
  - "all"
  - 0
```

This requires a recent ejabberd version that includes
[this commit](https://github.com/processone/ejabberd/commit/10481ed895016893ee9dc3fe23cd937fdc46ded6),
and `api_permissions` configured to allow mod_cron, for example:

```yaml
api_permissions:
  "console commands":
    from:
      - ejabberd_ctl
      - mod_cron
    who: all
    what: "*"
```

### Ctl

Use the option `ctl` and provide all `arguments` with quotes:

```yaml
ctl: delete_old_mam_messages
arguments:
  - "all"
  - "0"
```

### Erlang Function

Use `module`, `function`, and provide `arguments` in the correct format:

```yaml
module: ejabberd_auth
function: try_register
arguments:
  - "user1"
  - "localhost"
  - "somepass"
```

Please note the arguments in string format will be converted to binaries.
If the function expects strings, you can add the option `args_type: string`:

```yaml
module: mnesia
function: backup
args_type: string
arguments:
  - "/var/log/ejabberd/mnesia.backup"
```

Example Tasks
-------------

Those example tasks show how to specify arguments in the basic erlang formats:
```yaml
modules:
  mod_cron:
    tasks:
      - time: 30
        units: seconds
        module: erlang
        function: is_integer
        arguments:
          - 123456
      - time: 31
        units: seconds
        module: erlang
        function: is_float
        arguments:
          - 123.456
      - time: 32
        units: seconds
        module: erlang
        function: is_atom
        arguments:
          - 'this_is_atom'
      - time: 33
        units: seconds
        module: erlang
        function: is_atom
        arguments:
          - this_is_atom_too
      - time: 34
        units: seconds
        module: erlang
        function: is_binary
        arguments:
          - "Keep this as a binary"
      - time: 35
        units: seconds
        module: erlang
        function: is_list
        args_type: string
        arguments:
          - "Convert this as a string"
```

It is even possible to pass an argument that is a list of elements, see:
```yaml
modules:
  mod_cron:
    tasks:
      - time: 36
        units: seconds
        module: io
        function: format
        args_type: string
        arguments:
          - "log message, integer: ~p, float: ~p, atom: ~p, binary: ~p~n~n"
          - - 12345678
            - 123.456
            - atom_this_is
            - "this is a binary"
```

If you don't need to provide arguments at all,
you can remove `arguments`, or provide it with an empty list:
```yaml
modules:
  mod_cron:
    tasks:
      - time: 10
        units: seconds
        command: connected_users
      - time: 15
        units: seconds
        ctl: delete_expired_pubsub_items
      - time: 20
        units: seconds
        module: mod_pubsub
        function: delete_expired_items
        arguments: []
```

ejabberd Commands
-----------------

This module provides two new commands that can be executed using ejabberdctl:
* cron_list: list scheduled tasks
* cron_del taskid: delete this task from the schedule

Web Admin
---------

This module provides a page in the Host section of the Web Admin.
Currently that page only allows to view the tasks scheduled for that host.
