#!/usr/bin/env node
const proc = require('child_process');
const path = require('path');

const nodePath = path.join(__dirname,'/Release/zen-node.exe');
const workingDirectory = path.join(__dirname,'Release');

const appData =
    process.env.APPDATA ||
    (process.platform == 'darwin' ? path.join(process.env.HOME,'Library/Application Support') : path.join(process.env.HOME, '.config'));
const userData = path.join(appData, 'zen-node');

function start(args) {
  if (args === undefined)
    args = [];

  args.unshift('--data-path',userData);
  
  let node;

  if (process.platform !== "win32") {
    args.unshift(nodePath);
    node = proc.spawn('mono', args, {
        cwd: workingDirectory
    });
  }
  else {
    node = proc.spawn(nodePath,args, {
        cwd: workingDirectory
    });
  }

  return node
}

module.exports = start;