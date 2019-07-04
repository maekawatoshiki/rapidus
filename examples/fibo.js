function fibo(x) {
  if (x < 2) return 1
  return fibo(x - 1) + fibo(x - 2)
}

fibo(28)
