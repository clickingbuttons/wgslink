#!/usr/bin/env node

import { spawnSync } from 'node:child_process';
import { arch, platform } from 'node:os';
const name = 'wgslink';
const packageName = `@${name}/${arch()}-${platform()}`;
const binPath = `${packageName}/bin/${name}${platform() == 'win32' ? '.exe' : ''}`;
const absBinPath = require.resolve(binPath);
// const absBinPath = './zig-out/bin/wgslink';
// bundle('./test/boids-sprite-update.wgsl');

export interface Bundle {
	text: string;
	layout: any;
};

export function bundle(entry: string): Bundle {
	const child = spawnSync(absBinPath, ['--layout', entry], { encoding: 'utf8' });
	if (child.error) {
		throw child.error;
	} else if (child.status) {
		throw new Error(child.stderr);
	} else {
		return {
			text: child.stdout,
			layout: JSON.parse(child.stderr),
		};
	}
}

