var i, j
var ans = []
loop1: for (i = 0; i < 5; i++) {
  j = 0
  loop2: while (j < 3) {
    if (i === 1 && j === 1) {
      continue loop1
    }
    if (i === 2 && j === 1) {
      break loop2
    }
    if (i === 3 && j === 2) {
      break
    }
    if (i === 4 && j === 0) {
      j++
      continue
    }
    ans.push(i, j)
    j++
  }
}
console.log(ans)
ans
