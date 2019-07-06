let assert = (x, y) => { if (x !== y) throw "err" }

let total = 0, i = 1
while (i <= 20) {
  total += i
  if (i === 10) break
  i++
}
assert(total, 55)
