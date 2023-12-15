alias single = f32;
const pi_approx: single = 3.1415;
fn two_pi() -> single {
  return single(2) * pi_approx;
}

@vertex fn main() -> @builtin(position) vec4f {
	return vec4f(two_pi());
}
