Zen Protocol 
============

# Build 

## Linux

1. Install mono-devel from http://www.mono-project.com/download. If you choose to install via a package manager, add Mono's own repository first.
2. Install lmdb. The package name is liblmdb0 on Ubuntu and lmdb on Fedora.
3. Run the following:

```
./paket restore
chmod +x packages/zen_z3_linux/output/z3 
pushd src/Zulib
./build.fsx
popd
msbuild src
```

## OSX

1. Install Mono as in step 1 of the instructions for Linux.
2. Install lmdb. You can get it via brew with `brew install lmdb`.
3. Run the following:

```
./paket restore
chmod +x packages/zen_z3_osx/output/z3 
pushd src/Zulib
./build.fsx
popd
msbuild src
```

## Windows

Windows is not yet supported

# Run

## Linux and OSX

```
cd src/Node/bin/Debug
./zen-node
```

## Windows
```
cd src\Node\bin\Debug
zen-node.exe
```

# CLI

You can communicate with the node with `zen-cli`. Enter the `bin` directory of the node (`src/Node/bin/Debug`) and run: 

## Linux and OSX

```
./zen-cli --help
```

