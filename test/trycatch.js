let assert = require('./assert.js').deepStrictEqual

var a = []
const b = 1

function f() {
  const b = 2
  try {
    const b = 3
    a.push(0)
    try {
      throw 123
    } catch (e) {
      return e
    } finally {
    }
    throw 'bogus'
  } catch (e) {
    a.push(1)
  } finally {
    a.push(2)
  }
  a.push(5)
}

try {
  a.push(f())
  throw 10110
} catch (e) {
  a.push(e)
}

assert(a, [0, 2, 123, 10110])
