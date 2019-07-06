/* eslint-disable no-console */
function fibo(x) {
  if (x < 2) return 1
  return fibo(x - 1) + fibo(x - 2)
}

if (fibo(28) !== 514229) throw new Error()
