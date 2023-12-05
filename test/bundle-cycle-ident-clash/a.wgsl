// import { b } from './b.wgsl';
const a = 1.0 + b;

@vertex fn main() -> @location(0) vec4f {
	const res = vec4f(b + a);
	const b = 3.0;
	return res;
}

// ./b.wgsl b = 1
// ./a.wgsl a = 2
// ./a.wgsl main = 3

// module scope
// b: 1
// a: 2
// main: 3

// ref b: 1 (since scope == 0)
// ref a: 2 (since scope == 0)
// ref b: 1
// ref main: 3 (since scope == 0)

// scope 1
// res: 4
// b: 5

// ref res: 4
// ref b: 5
// ref b: 5
// ref res: 4
