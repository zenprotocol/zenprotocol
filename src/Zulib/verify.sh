#!/bin/sh

mono ../../packages/ZFStar/tools/fstar.exe \
     --smt ../../packages/zen_z3_linux/output/z3-linux \
     --prims fstar/prims.fst \
     --no_default_includes \
     --include ~/Dev/zenprotocol/src/Zulib/fstar \
     --cache_checked_modules \
     $@
