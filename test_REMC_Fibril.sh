#!/bin/bash

ghc -O3 -threaded -auto-all -prof -rtsopts tests/test_REMC_Fibril.hs && tests/test_REMC_Fibril +RTS -xc -N

