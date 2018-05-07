#!/bin/sh

rm -r Release
cd ../src/Zulib
./build.sh
cd ../..
rm -r src/Node/bin/Release
msbuild src /property:Configuration=Release /t:Node;Rebuild
cd package
cp -r ../src/Node/bin/Release .
cp ../packages/FSharp.Core/lib/net45/FSharp.Core.dll Release/
cp fstar.exe.config Release/

touch ./Release/.npmignore
npm pack .
