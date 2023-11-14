// test/boids-sprite-update.wgsl
@binding(0) @group(0) var<uniform> params: SimParams;
@binding(1) @group(0) var<storage,read> particlesA: Particles;
@binding(2) @group(0) var<storage,read_write> particlesB: Particles;
@compute @workgroup_size(64) fn main(@builtin(global_invocation_id) GlobalInvocationID: vec3<u32>) {
  var index: u32 = GlobalInvocationID.x;
  if (index >= arrayLength(&particlesA.particles)) {
    return;
  }
  var vPos = particlesA.particles[index].pos;
  var vVel = particlesA.particles[index].vel;
  var cMass = vec2<f32>(0.0, 0.0);
  var cVel = vec2<f32>(0.0, 0.0);
  var colVel = vec2<f32>(0.0, 0.0);
  var cMassCount: u32 = 0u;
  var cVelCount: u32 = 0u;
  var pos: vec2<f32>;
  var vel: vec2<f32>;
  for (var i: u32 = 0u; i < arrayLength(&particlesA.particles); i++) {
    if (i == index) {
      continue;
    }
    pos = particlesA.particles[i].pos.xy;
    vel = particlesA.particles[i].vel.xy;
    if (distance(pos, vPos) < params.rule1Distance) {
      cMass = cMass + pos;
      cMassCount = cMassCount + 1u;
    }
    if (distance(pos, vPos) < params.rule2Distance) {
      colVel = colVel - (pos - vPos);
    }
    if (distance(pos, vPos) < params.rule3Distance) {
      cVel = cVel + vel;
      cVelCount = cVelCount + 1u;
    }
  }
  if (cMassCount > 0u) {
    var temp = f32(cMassCount);
    cMass = (cMass / vec2<f32>(temp, temp)) - vPos;
  }
  if (cVelCount > 0u) {
    var temp = f32(cVelCount);
    cVel = cVel / vec2<f32>(temp, temp);
  }
  vVel = vVel + (cMass * params.rule1Scale) + (colVel * params.rule2Scale) + (cVel * params.rule3Scale);
  vVel = normalize(vVel) * clamp(length(vVel), 0.0, 0.1);
  vPos = vPos + (vVel * params.deltaT);
  if (vPos.x < -1.0) {
    vPos.x = 1.0;
  }
  if (vPos.x > 1.0) {
    vPos.x = -1.0;
  }
  if (vPos.y < -1.0) {
    vPos.y = 1.0;
  }
  if (vPos.y > 1.0) {
    vPos.y = -1.0;
  }
  particlesB.particles[index].pos = vPos;
  particlesB.particles[index].vel = vVel;
}