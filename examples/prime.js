function prime(n) {
  var k = 2
  while (k * k <= n) {
    if (n % k == 0) {
      return false
    }
    k = k + 1
  }
  return true
}

var i = 2
while (i < 40) {
  if (prime(i)) {
    console.log(i, "is prime")
  }
  i = i + 1
}
