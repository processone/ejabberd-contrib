defmodule ModLibcluster do
  use Ejabberd.Module

  def start(_host, opts) do

    strategy = case opts[:strategy] do
      :local_epmd ->
        :"Elixir.Cluster.Strategy.LocalEpmd"
      :epmd ->
        :"Elixir.Cluster.Strategy.Epmd"
      :kubernetes ->
        :"Elixir.Cluster.Strategy.Kubernetes"
      other_strategy ->
        other_strategy
    end
    info("Starting ejabberd module Libcluster with stategy #{inspect(strategy)}")

    config = [
      hosts: opts[:hosts],
      timeout: opts[:timeout]
    ]
    info("Starting ejabberd module Libcluster with config #{inspect(config)}")

    topologies = [
      ejabberd_cluster: [
        strategy: strategy,
        config: config,
        connect: {:ejabberd_admin, :join_cluster, []},
        disconnect: {:ejabberd_admin, :leave_cluster, []}
      ]
    ]
    children = [
      {Cluster.Supervisor, [topologies, [name: Ejabberd.ClusterSupervisor]]},
    ]
    Supervisor.start_link(children, strategy: :one_for_one, name: Ejabberd.Supervisor)
    info("Started ejabberd module Libcluster Demo")
    :ok
  end

  def stop(_host) do
    info("Stopping ejabberd module Libcluster Demo")
    :ok
  end

  def depends(_host, _opts) do
    []
  end

  def mod_opt_type(:hosts) do
    :econf.list(:econf.atom)
  end

  def mod_opt_type(:strategy) do
    :econf.atom
  end

  def mod_opt_type(:timeout) do
    :econf.either(:infinity, :econf.int());
  end

  def mod_options(_host) do
    [
     {:hosts, []},
     {:strategy, :local_epmd},
     {:timeout, :infinity}
    ]
  end

  def mod_doc() do
    %{:desc => "This is just an empty string."}
  end

end
