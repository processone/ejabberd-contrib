mod_prometheus - Prometheus metrics
===================================

- Requires: ejabberd 24.07 or higher, and Erlang/OTP 26 or higher
- Author: [Pouriya Jahanbakhsh](https://github.com/pouriya)

This module provides a web page with metrics suitable
for [Prometheus](https://prometheus.io/),
using the [prometheus](https://github.com/deadtrickster/prometheus.erl) erlang library.


Options
-------

The configurable options are:

- `mnesia: true | false`

  Enable mnesia metrics or not.
  Default: `false`

- `vm: [metric: true | false, ...]`

  Enable some Erlang Virtual Machine metrics.
  Available ones are:
  `memory`, `system_info`, `statistics`, `distribution`, `microstate_accounting`.
  For details please consult
  [prometheus.erl collectors](https://github.com/deadtrickster/prometheus.erl?tab=readme-ov-file#erlang-vm--otp-collectors).
  Default: `[]`

- `hooks: [Hook]`

  List of hooks to investigate.
  Default is an empty list: `[]`

  Each Hook is:
  - `hook: atom()`: the name of the hook
  - `type: counter | histogram`
  - `help: "Explanation"`
  - `stanza_label: true | false`
  - `host_label: true | false`
  - `collect: all | [Callback]`
      where each Callback is:
      - `module: atom()`
      - `function: atom()`
      - `help: "Explanation"`
      - `buckets: [integer()]`

Quick Start Guide
-----------------

### Install module

1. Start ejabberd

1. Download dependencies, compile and install this module:
   ```sh
   ejabberdctl module_install mod_prometheus
   ```
1. Check ejabberd provides metrics in the URL: http://localhost:5289/metrics/

### Start Prometheus and Grafana

Start Prometheus and Grafana, for example using Podman or Docker:

```sh
cd example
podman-compose up
# or
# docker-compose up
```

### Test Prometheus

1. Open in web browser http://localhost:9090/

1. Enter example query: `erlang_mnesia_tablewise_size{table="muc_online_room"}`

### Setup Grafana

1. Open in web browser http://localhost:3000/

1. Login with username `admin` and password `admin`

1. Add your first data source:
   - Add data source: `Prometheus`
   - Connection URL: `http://localhost:9090`
   - Click `Save & test`

1. Create your first dashboard
   - Import dashboard
   - Upload dashboard JSON file: you can try `ejabberd-dash.json`
   - prometheus: select the data source you created previously
   - Click `Import`
