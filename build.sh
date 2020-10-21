#!/bin/bash

rm -rf ./build
cabal new-install --installdir=./build --install-method=copy
