let assert = (x, y) => { if (x !== y) throw "err" }

let total = 0
for (let i = 1; i <= 20; i++) {
  total += i
  if (i === 10) break
}
assert(total, 55)
