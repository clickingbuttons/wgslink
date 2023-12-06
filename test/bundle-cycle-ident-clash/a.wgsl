// import { b } from './b.wgsl';
const a = 1.0 + b;

@vertex fn main() -> @location(0) vec4f {
	var res = vec4f(out_of_order + a);
	const out_of_order = 3.0;
	return res;
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
// res: 6
// out_of_order: 7

