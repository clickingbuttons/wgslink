// import { b } from './b.wgsl';

var a = 1.0 + b;

@vertex fn main() -> @builtin(position) vec4f {
	return vec4f(a);
}
