#!/bin/sh

cd $(dirname $0)/..

git submodule foreach git fetch
git submodule update --recursive --checkout --force
cabal install --enable-tests --only-dependencies --reinstall
