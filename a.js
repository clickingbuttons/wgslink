import { b } from './b.js';

const a = 1.0 + b;
{
	const a = 1 + b;
	// const b = 4 + a;
}
console.log(a + b);
