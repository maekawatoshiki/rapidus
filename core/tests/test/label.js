var ans = []

function f() {
  var i, j
  const k = 0
  loop1: for (i = 0; i < 5; i++) {
    const k = 1
    j = 0
    loop2: while (j < 3) {
      const k = 2
      {
        const k = 3
        {
          const k = 4
          if (i === 1 && j === 1) { continue loop1 }
          {
            const k = 5
            if (i === 4 && j === 0) {
              j++
              continue
            }
          }
        }
        if (i === 2 && j === 1) { break loop2 }
        if (i === 3 && j === 2) { break }
      }
      ans.push(i, j)
      j++
    }
  }
  ans.push(k)
}
f()
console.log(ans)
ans
