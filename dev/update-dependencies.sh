#!/bin/sh

cd $(dirname $0)/..

git submodule init
git submodule foreach git fetch
git submodule update --recursive --checkout --force

$(dirname $0)/add-sources.sh

cabal install --enable-tests --only-dependencies --reinstall
