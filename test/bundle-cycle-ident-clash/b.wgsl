// import { c as cc } from './c.wgsl';
const b = 2.0 + cc + a;
const a = 1.0;

// ./c.wgsl c = 4
// ./b.wgsl b = 1 SKIP
// ./b.wgsl a = 5

// module scope
// cc: 6
// b: 1
// a: 7

// ref c:  (since scope == 0)
// ref b:  (since scope == 0)
// ref c:
// ref a:
// ref a:  (since scope == 0)
