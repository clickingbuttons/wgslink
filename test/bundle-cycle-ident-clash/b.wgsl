// import { c as cc } from './c.wgsl';
const b = 2.0 + cc + a;
const a = 1.0;

// module symbols
// ./c.wgsl c = 4
// ./b.wgsl b = 1 HIT
// ./b.wgsl a = 5

// module scope
// cc: 4
// b: 1
// a: 7

