var i = 2
while (i < 30) {
  k = 2
  p = 1
  while (k * k <= i) {
    if (i % k == 0) {
      p = 0
      k = i
    }
    k = k + 1
  }
  if (p == 1) console.log(i) 
  i = i + 1
}
