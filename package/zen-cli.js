#!/usr/bin/env node

const proc = require('child_process');
const path = require('path');

const nodePath = path.join(__dirname,'/Release/zen-cli.exe');
const workingDirectory = path.join(__dirname,'Release');

function start(args) {
    if (args === undefined)
        args = [];

    let node;

    if (process.platform !== "win32") {
        args.unshift(nodePath);
        node = proc.spawn('mono', args, {
            cwd: workingDirectory
        });
    }
    else {
        node = spawn(nodePath,args, {
            cwd: workingDirectory
        });
    }

    return node
}

const node = start(process.argv.slice(2));

node.stdout.pipe(process.stdout);
node.stderr.pipe(process.stderr);

node.on('exit', function (code) {
    process.exit(code);
});
