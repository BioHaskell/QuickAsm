#!/bin/bash

#ghc -rtsopts -prof -auto-all -caf-all tests/test_REMC.hs && time tests/test_REMC +RTS -Pa -hc -xc -H150M
ghc -threaded -rtsopts tests/test_REMC.hs && time tests/test_REMC +RTS -H150M -N3
#ghc -threaded -rtsopts -prof -auto-all -caf-all tests/test_REMC.hs && time tests/test_REMC +RTS -Pa -hc -xc -H150M -N2
#ghc -rtsopts -prof -auto-all -caf-all tests/test_REMC.hs && time tests/test_REMC +RTS -K1K -Pa -hc -xc

