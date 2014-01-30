#!/bin/bash

set -e

cabal sandbox init
cabal clean
cabal install --enable-tests --only-dependencies
cabal configure --enable-tests
cabal build

./dist/build/SSALabels/SSALabels --jxml=./dist/test-results.xml
