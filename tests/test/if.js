let assert = (x, y) => { if (x !== y) throw "err" }

let i = 1;
if (i + 1 == 2) {
  i = 10
} else {
  i = 20
}
assert(i, 10)
if (i + 1 != 2) i = 3
assert(i, 3)
