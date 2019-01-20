function fact(n) {
  if (n < 2) { return 1 } else { return n * fact(n - 1) }
}
var a = []
var f = fact(12)
console.log(f)
a.push(f)
a
