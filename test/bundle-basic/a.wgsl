// import { b } from './b.wgsl';
fn foo() -> f32 {
	return 4.0;
}
const a = 1.0 + b + foo();

@vertex fn main() -> @builtin(position) vec4f {
	return vec4f(a);
}
