// import { b } from './b.wgsl';
// import { c } from './c.wgsl';

@vertex fn main() -> @location(0) vec4f {
	var a: u32 = b + c;
	return vec4f(f32(a));
}
