#!/bin/bash

runghc scripts/reconstuct.hs examples/reconstruct/best.out test.pdb

if [ -r test.pdb ]; then
  pymol test.pdb \
	examples/reconstruct/orig.pdb \
        examples/reconstruct/test.pml
fi

runghc scripts/reconstructBest.hs examples/reconstruct/default.out newDecoy.pdb

