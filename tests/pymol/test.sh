#!/usr/bin/pymol

(cd ../..;
runghc tests/test_Topo.hs|grep ^ATOM > tests/pymol/test.pdb)

pymol test.pdb start.pdb test.pml
