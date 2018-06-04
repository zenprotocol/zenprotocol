#!/bin/sh

chmod +x ../../packages/zen_z3_linux/output/z3-linux
chmod +x ../../packages/zen_z3_osx/output/z3-osx

cp ../../packages/zulib/System.Reflection.Metadata/lib/portable-net45+win8/System.Reflection.Metadata.dll ../../packages/FAKE/tools/ 
cp Zen.FSharp.Compiler.Service.dll.config ../../packages/FAKE/tools/
cp Zen.FSharp.Compiler.Service.dll.config ../../packages/Zen.FSharp.Compiler.Service/lib/net45/
cp FAKE.exe.config ../../packages/FAKE/tools/

exit_code=$?
  if [ $exit_code -ne 0 ]; then
    exit $exit_code
  fi

ulimit -n 2000
mono ../../packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx
