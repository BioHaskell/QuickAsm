#!/bin/bash

runghc tests/reconstuct.hs examples/reconstruct/best.out test.pdb

if [ -r test.pdb ]; then
  pymol test.pdb \
	examples/reconstruct/orig.pdb \
        examples/reconstruct/test.pml
fi
