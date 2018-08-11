function prime(n) {
  var k = 2
  while (k * k <= n) {
    if (n % k == 0) {
      return false
    }
    k += 1
  }
  return true
}

var i = 2
while (i < 100000) {
  if (prime(i)) {
    console.log(i, "is prime")
  }
  i += 1
}
