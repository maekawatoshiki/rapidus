var assert = require('assert').deepEqual
const i = 100
let j = 0
for (let i = 0; i < 5; i++) {
  {
    const i = 4
    {
      const i = 7
      j += i
    }
  }
}
assert(i,100)
assert(j,35)