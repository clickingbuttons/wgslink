// import { b } from './b.wgsl';
const a = 1.0 + b;
const unused = 1.0 + a + b;

@vertex fn main() -> @location(0) vec4f {
	return vec4f(a);
}
