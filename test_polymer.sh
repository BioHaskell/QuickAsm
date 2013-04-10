#!/bin/bash

#ghc -O0 -fforce-recomp -prof -auto-all -caf-all tests/test_Polymer.hs && tests/test_Polymer
#ghc -O0 -rtsopts -prof -auto-all -caf-all tests/test_Polymer.hs && tests/test_Polymer +RTS -K1K
ghc --make -fforce-recomp -rtsopts -prof -auto-all -caf-all tests/test_Polymer && tests/test_Polymer +RTS -Stest_Polymer.gc -B -hr -H250M -Pa "-m30%"

