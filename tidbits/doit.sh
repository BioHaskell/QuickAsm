#!/bin/bash

DECOY=../../hRosetta/examples/silent/S_0012_6592.pdb

(egrep ' C | CA | N | O ' ${DECOY} | cut -c13-21,23-56 | head -32) > header.txt &&

./doit.py > header.hs

