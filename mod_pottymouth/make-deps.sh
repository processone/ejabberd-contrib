#!/bin/sh

PWD=`pwd`

cd ~/.ejabberd-modules/sources/ejabberd-contrib/mod_pottymouth/
mkdir deps
cd deps
git clone https://github.com/madglory/etbloom.git
cd etbloom
rm -rf deps/
./rebar get-deps
cp ../../rebar.config.bitarray deps/bitarray/rebar.config
./rebar compile
cp -R deps/bitarray/ebin ../../
cp -R deps/bitarray/priv ../../
cp -R deps/proper/ebin ../../

cd $PWD
