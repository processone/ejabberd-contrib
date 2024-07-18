mod_libcluster - Join nodes into cluster
========================================

Requires:
- ejabberd 24.07 or higher compiled with Elixir support


This small module connects this ejabberd node to other nodes
using [libcluster](https://github.com/bitwalker/libcluster).

Options
-------

This module supports several configurable options.
There are some example configuration files in the `test/` directory.
Options supported by this module:

* `strategy`

  Sets the clustering strategy to use.

  Supported values are:
  - `local_epmd`: connect to all nodes managed by the local epmd program
  - `epmd`: connect to the nodes you specify in the `config: hosts` option
  - `kubernetes`

  Default value: `local_epmd`.

* `hosts`

  List of erlang node names to connect.
  This is required, and only useful, when using the `epmd` strategy.
  Default value: `[]`.

* `timeut`

  Timeout to connect to a node, expressed in milliseconds.
  This is only useful when using the `epmd` strategy.
  Default value: `infinity`.

For details of options supported please check the
[libcluster documentation](https://hexdocs.pm/libcluster).

Test
----

There's a test script in `test/test.sh` that you can run.
It compiles ejabberd, installs in `/tmp/` several nodes,
and builds a cluster using the desired strategy.
