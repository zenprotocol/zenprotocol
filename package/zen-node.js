#!/usr/bin/env node
const start = require('./index.js');

const node = start(process.argv);

node.stdout.pipe(process.stdout);
node.stderr.pipe(process.stderr);

node.on('exit', function (code) {
  console.log('child process exited with code ' + code.toString());
  process.exit(code);
});
