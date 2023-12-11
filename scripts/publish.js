#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const scope = '@wgslink';
const name = 'wgslink';
const packages_dir = 'npm';
const repo = 'https://github.com/clickingbuttons/wgslink';
const version = fs.readFileSync('build.zig.zon', 'utf8').match(/.version = "(.*)"/)[1]
console.log('publishing', version);

const zigTargets = [
	'arm-linux',
	'aarch64-linux',
	'x86_64-linux',
	'aarch64-macos',
	'x86_64-macos',
	'aarch64-windows',
	'x86_64-windows',
];

const zigToNodeOS = {
	'linux': 'linux',
	'windows': 'win32',
	'macos': 'darwin',
};

const zigToNodeCPU = {
	'arm': 'arm',
	'aarch64': 'arm64',
	'x86_64': 'x64',
};

const packageName = t => {
	const split = t.split('-');
	const cpu = zigToNodeCPU[split[0]];
	const os = zigToNodeOS[split[1]];
	return `${scope}/${cpu}-${os}`;
}

function writeNPMFiles(package_json) {
	const package_dir = path.join(packages_dir, package_json.name);
	fs.mkdirSync(package_dir, { recursive: true });

	const package_json_path = path.join(package_dir, "package.json");
	fs.writeFileSync(package_json_path, JSON.stringify(package_json, null, 2));

	const readme_path = path.join(package_dir, "README.md");
	fs.writeFileSync(readme_path, `# ${package_json.name}\nA fast platform-specific build of ${repo}`);

	const license_path = path.join(package_dir, "LICENSE.md");
	fs.copyFileSync("LICENSE.md", license_path);
}

function buildTarget(t) {
	const split = t.split('-');
	const cpu = zigToNodeCPU[split[0]];
	const os = zigToNodeOS[split[1]];
	const package_json = {
		name: packageName(t),
		version,
		description: `${t} target of wgslink, a WGSL bundler and minifier.`,
		repository: { url: `git+${repo}.git` },
		license: 'MIT',
		preferUnplugged: true, // Keeps yarn from compressing binary
		engines: { node: '>=12' },
		scripts: {}, // Keeps `npm publish` from crying
		cpu: [cpu],
		os: [os],
	};
	writeNPMFiles(package_json);
	const package_dir = path.join(packages_dir, package_json.name);

	console.log("building", package_dir);

	execSync(`zig build -Doptimize=ReleaseFast -Dtarget=${t} -p ${package_dir}`);
}

function buildRoot() {
	const package_json = {
		name,
		version,
		description: 'A WGSL bundler and minifier.',
		repository: { url: `git+${repo}.git` },
		license: 'MIT',
		engines: { node: '>=12' },
		scripts: {}, // Keeps `npm publish` from crying
		bin: { "wgslink": "bin/wgslink" },
		optionalDependencies: zigTargets.reduce((acc, cur) => {
			acc[packageName(cur)] = version;
			return acc;
		}, {}),
	};
	writeNPMFiles(package_json);
	const package_dir = path.join(packages_dir, package_json.name);

	const bin_dir = path.join(package_dir, "bin");
	fs.mkdirSync(bin_dir, { recursive: true });
	const bin_path = path.join(bin_dir, "wgslink");
	fs.copyFileSync(path.join(__dirname, "cli.js"), bin_path);
	fs.chmodSync(bin_path, 0o755);
}

function publishPackage(dir) {
	console.log("publishing", dir);

	try {
		execSync('npm publish --access public', { cwd: dir });
	} catch {
		// Ignore republishing. Check logs if fails.
	}
}

function publishTarget(t) {
	const package_dir = path.join(packages_dir, packageName(t));
	publishPackage(package_dir);
}

function publishRoot() {
	const package_dir = path.join(packages_dir, name);
	publishPackage(package_dir)
}

zigTargets.forEach(buildTarget);
buildRoot();

zigTargets.forEach(publishTarget);
publishRoot();
