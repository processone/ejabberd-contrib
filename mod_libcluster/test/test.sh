#!/bin/bash

COLOUR="\e[1;49;33m"
NEUTRAL="\033[00m"

log() {
    echo ""
    echo -e $COLOUR"==> $1"$NEUTRAL
}

MOD=$(pwd)

TOOL=$1
STR=$2
SRC=$3
if [ -n "$TOOL" ] && [ -n "$STR" ] && [ -n "$SRC" ] ; then
    log "Using tool: $TOOL"
    log "Using strategy: $STR"
    log "Using ejabberd source code from: $SRC"
else
    echo "Usage: $0 <tool> <strategy> <path>"
    echo "  <tool> = mix | rebar3"
    echo "  <strategy> = local_epmd | epmd"
    echo "  <path> = path to the ejabberd source code"
    echo "For example: $0 mix local_epmd /home/user1/git/ejabberd/"
    exit 1
fi

cd "$SRC" || exit 1

log "Compile ejabberd release..."

compile() {
    ./autogen.sh
    ./configure \
        --with-rebar=$TOOL \
        --enable-all
    make
    rm _build/prod/*.tar.gz
    make rel
}

compile

log "Preparing ejabberd nodes..."

case "$TOOL" in
    mix)
        echo "Setting up 'mix' tool..."
        FILE=$(ls -1 _build/prod/*.tar.gz)
        ;;
    rebar3)
        echo "Setting up 'rebar3' tool..."
        FILE=$(ls -1 _build/prod/rel/ejabberd/*.tar.gz)
        ;;
esac

create_node() {
    N=$1
    log "Preparing node $N using file $FILE..."
    rm -rf /tmp/libcluster/"$N"
    mkdir -p /tmp/libcluster/"$N"
    tar -xzf "$FILE" -C /tmp/libcluster/"$N"
    sed -i "s|#' POLL|EJABBERD_BYPASS_WARNINGS=true\n\n#' POLL|g" /tmp/libcluster/"$N"/conf/ejabberdctl.cfg
    sed -i "s|#ERLANG_NODE=.*|ERLANG_NODE=ejabberd$N@127.0.0.1|" /tmp/libcluster/"$N"/conf/ejabberdctl.cfg
    sed -i "s| port: \([0-9]*\)| port: \1$N|g" /tmp/libcluster/"$N"/conf/ejabberd.yml
    sed -i "s|mod_proxy65:|mod_proxy65:\n    port: 777$N|" /tmp/libcluster/"$N"/conf/ejabberd.yml
    node[N]=/tmp/libcluster/"$N"/bin/ejabberdctl
}

create_node 1
create_node 2
create_node 3

FIRST=${node[1]}
SECOND=${node[2]}
THIRD=${node[3]}

log "Let's start the first node..."

echo "first: $FIRST"
$FIRST start
$FIRST started
$FIRST set_master self

log "Uninstall and install mod_libcluster:"

$FIRST module_uninstall mod_libcluster
rm -rf $MOD/deps/
rm -rf $MOD/ebin/
$FIRST module_install mod_libcluster || exit 1

log "Configure mod_libcluster:"

case "$STR" in
    local_epmd)
        echo "Setting up 'local_epmd' config..."
        cp "$MOD"/test/local_epmd.yml \
           "$HOME"/.ejabberd-modules/mod_libcluster/conf/mod_libcluster.yml
        ;;
    epmd)
        echo "Setting up 'epmd' config..."
        cp "$MOD"/test/epmd.yml \
           "$HOME"/.ejabberd-modules/mod_libcluster/conf/mod_libcluster.yml
        ;;
esac

log "Initial cluster of first node (should be just this node):"

$FIRST list_cluster

log "Let's start the second node, it should connect automatically to the first node..."

$SECOND start
$SECOND started
sleep 5 # wait a few seconds to let the second node join the cluster properly
$SECOND set_master ejabberd1@127.0.0.1

log "Let's start the third node, it should connect automatically to the first node..."

$THIRD start
$THIRD started
sleep 5 # wait a few seconds to let the second node join the cluster properly

log "Cluster as seen by first node:"

$FIRST list_cluster

log "Cluster as seen by second node:"

$SECOND list_cluster

log "Cluster as seen by third node:"

$THIRD list_cluster

log "Stop first node, and check cluster as seen by second node:"

$FIRST stop
$FIRST stopped
$SECOND list_cluster

log "... as seen by third node:"

$THIRD list_cluster

log "Start again first node, and check cluster as seen by all the nodes:"

$FIRST start
$FIRST started
sleep 5
echo "First: "
$FIRST list_cluster
echo "Second: "
$SECOND list_cluster
echo "Third: "
$THIRD list_cluster

log "Stopping all nodes..."

$FIRST stop
$SECOND stop
$THIRD stop
