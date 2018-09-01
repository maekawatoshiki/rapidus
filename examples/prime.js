function prime(n) {
  if (n % 2 == 0) return false;
  for (var k = 3; k * k <= n; k += 2) 
    if (n % k == 0) 
      return false;
  return true;
}

console.log("2 is prime");
for (var i = 2; i < 1000000; i += 1) {
  if (prime(i)) console.log(i, "is prime");
}
