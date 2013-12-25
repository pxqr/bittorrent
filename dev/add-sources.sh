#!/bin/bash

for s in $(ls $(dirname $0)/../sub); do
    (cd $(dirname $0)/.. && cabal sandbox add-source sub/$s)
done
