#!/bin/bash

# Upload this binary to github releases

rm -rf ./build
cabal new-install --installdir=./build --install-method=copy
