```
stack runhaskell --package extra \
                 --package optparse-applicative \
                 CI.hs -- --da \
                          --merge-base-sha=ghc-8.8.1-release \
                          --patch=upstream/da-master-8.8.1 \
                          --patch=upstream/da-unit-ids-8.8.1 \
                          --gen-flavor=da-ghc-8.8.1 \
                          --upstream=/home/matt/dev/da-ghcfor
```


```
sudo apt-get install build-essential libgmp-dev autoconf
```
