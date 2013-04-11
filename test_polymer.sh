#!/bin/bash

#ghc -O0 -fforce-recomp -prof -auto-all -caf-all tests/test_Polymer.hs && tests/test_Polymer
#ghc -O0 -rtsopts -prof -auto-all -caf-all tests/test_Polymer.hs && tests/test_Polymer +RTS -K1K
#ghc --make -fforce-recomp -rtsopts -prof -auto-all -caf-all tests/test_Polymer && tests/test_Polymer +RTS -Stest_Polymer.gc -B -hr -H250M -nG 1 -G 4 -Pa "-m30%"
ghc --make -threaded -rtsopts -prof -auto-all -caf-all tests/test_Polymer && tests/test_Polymer +RTS -Stest_Polymer.gc -B -hr -H1G -qg1 -G4 -Pa "-m30%"

