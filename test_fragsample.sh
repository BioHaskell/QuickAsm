#!/bin/bash

ghc -O3 scripts/FragSample && \
scripts/FragSample examples/assembly/aat000_09.200_R3 examples/assembly/good.out examples/assembly/asyn_gs_long_bb.newcst current2.out best2.out best2.pdb
