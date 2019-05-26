let assert = (x, y) => {
  if (x !== y) throw 'err'
}

function fibo(x) {
  if (x < 2) return 1
  return fibo(x - 1) + fibo(x - 2)
}

assert(fibo(10), 89)
