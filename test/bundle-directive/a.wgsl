// import { b } from './b.wgsl';
enable foo,bar;
requires feat1,feat2;
diagnostic(off,derivative_uniformity);

@vertex fn main() -> @builtin(position) vec4f {
	return vec4f(b);
}
