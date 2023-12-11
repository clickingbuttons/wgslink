#!/usr/bin/env node

const child_process = require('child_process');
const os = require('os');
const name = 'wgslink';
const packageName = `@${name}/${os.arch()}-${os.platform()}`;
const binPath = `${packageName}/bin/${name}${os.platform() == 'win32' ? '.exe' : ''}`;
const absBinPath = require.resolve(binPath);

child_process.execFileSync(absBinPath, process.argv.slice(2), { stdio: 'inherit', });
