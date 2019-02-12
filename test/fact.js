function fact(n) {
  //console.log("fact " + n)
  if (n < 2) { return 1 } else {
    return n * fact(n - 1) }
}
//__enableJit(false)
var assert = require('assert').deepEqual
assert(fact(12), 479001600)
