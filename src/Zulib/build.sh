#!/bin/sh

mono ../../.paket/paket.exe restore
chmod +x ../../packages/zen_z3_linux/output/z3

exit_code=$?
  if [ $exit_code -ne 0 ]; then
    exit $exit_code
  fi

ulimit -n 2000
mono ../../packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx
