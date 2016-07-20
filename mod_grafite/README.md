
mod_grafite - Gathers statistics and publishes via statsd/grafite
author: Thiago Rocha Camargo (rochacamargothiago@gmail.com)  

	mod_grafite
	==============

Gathers statistics and publishes via statsd/grafite

	CONFIGURE
	---------

Enable the module in ejabberd.yml for example with a basic configuration:
modules:
  mod_grafite:
    statsdhost: "carbonr.xmpp.com.br"
    statsdport: 8125

Configurable options:
  statsdhost: Host of the statsd server
  statsdport: Port of the statsd server

	EXAMPLE CONFIGURATION
	---------------------

modules:
  mod_grafite:
    statsdhost: "carbonr.xmpp.com.br"
    statsdport: 8125

	FEATURE REQUESTS
	----------------

 - Add support for configurable Hooks

