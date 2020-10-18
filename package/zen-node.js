#!/usr/bin/env node
const start = require('./index.js');

const args = process.argv.slice(2);

if (args.includes('--version')) {
  logVersion();
  process.exit(0);
}

const node = start(args);

console.log('Default path is: ' + args[2]);

node.stdout.pipe(process.stdout);
node.stderr.pipe(process.stderr);

node.on('exit', function (code) {
  console.log('Closed');
  process.exit(code);
});

process.on('SIGINT', function () {
    console.log('Ctrl+C pressed');
    node.kill('SIGINT');
});

function logVersion() {
  const packageJson = require('./package.json')
  console.log('zen-node version: ', packageJson.version)
}
