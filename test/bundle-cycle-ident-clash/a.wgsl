// import { b } from './b.wgsl';
const a = 1.0 + b;

@vertex fn main() -> @builtin(position) vec4f {
	const b = 2.0;
	return vec4f(a + b);
}

// module symbols
// ./b.wgsl b = 1
// ./a.wgsl a = 2
// ./a.wgsl main = 3

// module scope
// b: 1
// a: 2
// main: 3

// scope 1
// b: 6
