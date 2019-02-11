function f(x) {
  try {
    if (x === 1) throw 10
    return x * f(x - 1)
  } catch (e) {
    return 1
  }
}

console.log(f(100))
