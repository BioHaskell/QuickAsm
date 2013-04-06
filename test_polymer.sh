#!/bin/bash

#ghc -O0 -fforce-recomp -prof -auto-all -caf-all tests/test_Polymer.hs && tests/test_Polymer
ghc -O0 -prof -auto-all -caf-all tests/test_Polymer.hs && tests/test_Polymer
