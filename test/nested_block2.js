const assert = require('assert').deepEqual
const i = 100
function f(g) {
  const i = 0
  try {
    while (true) {
      const i = 1
      while (true) {
        const i = 2
        while (true) {
          const i = 3
          throw 100
        }
      }
    }
  } catch (e) {
    assert(i, 0)
    while (true) {
      const i = 5
      while (true) {
        const i = 6
        //break label
        return e - 10
      }
    }
  } finally {
    label: while (true) {
      const i = 7
      while (true) {
        const i = 8
        break label
      }
    }
    assert(i, 0)
  }
}
assert(i, 100)
assert(f(), 90)
