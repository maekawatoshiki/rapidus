function fact(n) {
  if (n < 2) { return 1 } else { return n * fact(n - 1) }
}

console.log("fact(10) =", fact(10))
