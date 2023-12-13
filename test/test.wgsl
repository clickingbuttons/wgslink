struct type_2 {
    member: vec2<f32>,
    member_1: vec2<f32>,
}

struct type_3 {
    member: f32,
    member_1: f32,
    member_2: f32,
    member_3: f32,
    member_4: f32,
    member_5: f32,
    member_6: f32,
}

struct type_5 {
    member: array<type_2>,
}

struct type_9 {
    member: type_3,
}

@group(0) @binding(0)
var<uniform> global: type_9;
@group(0) @binding(1)
var<storage> global_1: type_5;
@group(0) @binding(2)
var<storage, read_write> global_2: type_5;
var<private> global_3: vec3<u32>;

fn foo() {
    var local: f32 = f32();
    var local_1: vec2<f32> = vec2<f32>();
    var local_2: u32 = 0u;
    var local_3: vec2<f32> = vec2<f32>(0.0, 0.0);
    var local_4: u32 = u32();
    var local_5: u32 = 0u;
    var local_6: u32 = 0u;
    var local_7: vec2<f32> = vec2<f32>(0.0, 0.0);
    var local_8: vec2<f32> = vec2<f32>();
    var local_9: f32 = f32();
    var local_10: vec2<f32> = vec2<f32>();
    var local_11: vec2<f32> = vec2<f32>(0.0, 0.0);
    var local_12: vec2<f32> = vec2<f32>();

    let _e53 = global_3;
    local_4 = _e53.x;
    let _e56 = local_4;
    if (_e56 >= arrayLength((&global_1.member))) {
        return;
    }
    let _e60 = local_4;
    let _e64 = global_1.member[_e60].member;
    local_8 = _e64;
    let _e65 = local_4;
    let _e69 = global_1.member[_e65].member_1;
    local_12 = _e69;
    loop {
        let _e70 = local_5;
        if (_e70 < arrayLength((&global_1.member))) {
        } else {
            break;
        }
        let _e74 = local_5;
        let _e75 = local_4;
        if (_e74 == _e75) {
            continue;
        }
        let _e77 = local_5;
        let _e81 = global_1.member[_e77].member;
        local_10 = _e81.xy;
        let _e83 = local_5;
        let _e87 = global_1.member[_e83].member_1;
        local_1 = _e87.xy;
        let _e89 = local_10;
        let _e90 = local_8;
        let _e93 = global.member.member_1;
        if (distance(_e89, _e90) < _e93) {
            let _e95 = local_3;
            let _e96 = local_10;
            local_3 = (_e95 + _e96);
            let _e98 = local_2;
            local_2 = (_e98 + 1u);
        }
        let _e100 = local_10;
        let _e101 = local_8;
        let _e104 = global.member.member_2;
        if (distance(_e100, _e101) < _e104) {
            let _e106 = local_11;
            let _e107 = local_10;
            let _e108 = local_8;
            local_11 = (_e106 - (_e107 - _e108));
        }
        let _e111 = local_10;
        let _e112 = local_8;
        let _e115 = global.member.member_3;
        if (distance(_e111, _e112) < _e115) {
            let _e117 = local_7;
            let _e118 = local_1;
            local_7 = (_e117 + _e118);
            let _e120 = local_6;
            local_6 = (_e120 + 1u);
        }
        continue;
        continuing {
            let _e122 = local_5;
            local_5 = (_e122 + 1u);
        }
    }
    let _e124 = local_2;
    if (_e124 > 0u) {
        let _e126 = local_2;
        local_9 = f32(_e126);
        let _e128 = local_3;
        let _e129 = local_9;
        let _e130 = local_9;
        let _e133 = local_8;
        local_3 = ((_e128 / vec2<f32>(_e129, _e130)) - _e133);
    }
    let _e135 = local_6;
    if (_e135 > 0u) {
        let _e137 = local_6;
        local = f32(_e137);
        let _e139 = local_7;
        let _e140 = local;
        let _e141 = local;
        local_7 = (_e139 / vec2<f32>(_e140, _e141));
    }
    let _e144 = local_12;
    let _e145 = local_3;
    let _e147 = global.member.member_4;
    let _e150 = local_11;
    let _e152 = global.member.member_5;
    let _e155 = local_7;
    let _e157 = global.member.member_6;
    local_12 = (((_e144 + (_e145 * _e147)) + (_e150 * _e152)) + (_e155 * _e157));
    let _e160 = local_12;
    let _e162 = local_12;
    local_12 = (normalize(_e160) * clamp(length(_e162), 0.0, 0.1));
    let _e166 = local_8;
    let _e167 = local_12;
    let _e169 = global.member.member;
    local_8 = (_e166 + (_e167 * _e169));
    let _e173 = local_8[0u];
    if (_e173 < -1.0) {
        local_8[0u] = 1.0;
    }
    let _e177 = local_8[0u];
    if (_e177 > 1.0) {
        local_8[0u] = -1.0;
    }
    let _e181 = local_8[1u];
    if (_e181 < -1.0) {
        local_8[1u] = 1.0;
    }
    let _e185 = local_8[1u];
    if (_e185 > 1.0) {
        local_8[1u] = -1.0;
    }
    let _e188 = local_4;
    let _e189 = local_8;
    global_2.member[_e188].member = _e189;
    let _e193 = local_4;
    let _e194 = local_12;
    global_2.member[_e193].member_1 = _e194;
    return;
}

@compute @workgroup_size(64, 1, 1)
fn main(@builtin(global_invocation_id) param: vec3<u32>) {
    global_3 = param;
    foo();
}
