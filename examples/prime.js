function prime(n) {
  if (n % 2 == 0) return false;

  var k = 3;
  while (k * k <= n) {
    if (n % k == 0) {
      return false;
    }
    k += 2
  }
  return true;
}

var i = 2;
while (i < 1000000) {
  if (prime(i)) {
    console.log(i, "is prime");
  }
  i += 1;
}
