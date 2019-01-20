function fact(n) {
  if (n < 2) {
    return 1
  } else {
    return n * fact(n - 1)
  }
}

var assert = require('assert').deepEqual
assert(fact(20), 479001600)
