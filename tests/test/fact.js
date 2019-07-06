let assert = (x, y) => {
  if (x !== y) throw 'err'
}

function fact(n) {
  if (n < 2) {
    return 1
  } else {
    return n * fact(n - 1)
  }
}

assert(fact(12), 479001600)
