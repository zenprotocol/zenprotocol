#!/bin/sh

rm -r Release
cd ../src/Zulib
./build.sh
cd ../..
rm -r src/Node/bin/Release
msbuild src /property:Configuration=Release
cd package
cp -r ../src/Node/bin/Release .
touch ./Release/.npmignore
npm pack .
