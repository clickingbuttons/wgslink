#!/usr/bin/env node

import { execFileSync } from 'node:child_process';
// const os = require('os');
// const packageName = `@wgslink/wgslink-${nodeToZigCPU[os.arch()]}-${nodeToZigOS[os.platform()]}`;
//
// const binPath = `${packageName}/bin/usfm${os.platform() == 'win32' ? '.exe' : ''}`;
// const absBinPath = require.resolve(binPath);
const absBinPath = './zig-out/bin/wgslink';

export interface Options {
	entries: string[],
	outdir: string,
};
export function bundle(opts: Options) {
	opts.entries.forEach(e => {
		try {
			execFileSync(absBinPath, ['--layout', '--outdir', opts.outdir, e], {
				stdio: 'inherit',
				windowsHide: true,
			});
		} catch {}
	});
}

// bundle({
// 	entries: ['./test/boids-sprite-update.wgsl'],
// 	outdir: 'out',
// });
