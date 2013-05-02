#!/bin/bash

ghc -O3 -threaded tests/test_REMC_Fibril.hs &&
tests/test_REMC_Fibril +RTS -N

