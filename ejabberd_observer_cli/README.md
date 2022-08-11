ejabberd observer_cli plugins
=============================

[observer_cli][oc] is an erlang tool to
visualize Erlang node statistics on the command line.

This directory contains several plugins
specifically written to view statistics of [ejabberd][ej].

To install this, just run:
```bash
ejabberdctl module_install ejabberd_observer_cli
```
It will automatically download, compile and install the required dependencies:
[observer_cli][oc], [recon][recon],
and the additional plugin [os_stats][os].

Then, start an erlang shell attached to your ejabberd node, for example:
```bash
ejabberdctl debug
```
in that erlang shell execute:
```erlang
ejabberd_observer_cli:start().
```

If using Elixir (for example when started with `ejabberdctl iexdebug`, run:
```elixir
:ejabberd_observer_cli.start()
```

To sort columns or change between the different pages,
type the corresponding letter and hit Enter.
For example, to view MUC statistics, type: `M and then Enter`.

There is no configuration required.

[oc]: https://github.com/zhongwencool/observer_cli
[os]: https://github.com/zhongwencool/os_stats
[recon]: https://github.com/ferd/recon
[ej]: https://www.ejabberd.im/
