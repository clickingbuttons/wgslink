// import { b } from './b.wgsl';
const a = 1.0 + b;

@vertex fn main() -> @location(0) vec4f {
	return vec4f(a);
}
