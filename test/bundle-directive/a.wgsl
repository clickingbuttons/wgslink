// import { b } from './b.wgsl';
enable foo,bar;
requires feat1,feat2;
diagnostic(off,derivative_uniformity);

@vertex fn main() -> @location(0) vec4f {
	return vec4f(b);
}
