let assert = (x, y) => { if (x !== y) throw "err" }

let a = 1, b = 10

function f() {
  let a = 2, b = 20
  assert(a, 2)
  {
    let a = 3
    assert(a, 3)
    assert(b, 20)
  }
  assert(a, 2)
}

f()

assert(a, 1)
