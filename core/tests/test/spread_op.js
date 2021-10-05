let assert = require('./tests/assert').deepStrictEqual

let a = [1, 2]
let b = [4, 5]
let c = { a, b }

assert([0, ...a, 3], [0, 1, 2, 3])
assert([...a, 3], [1, 2, 3])
assert([0, ...a], [0, 1, 2])
assert([0, ...a, ...b, 6], [0, 1, 2, 4, 5, 6])
assert([0, ...c.b, ...c.a, 6], [0, 4, 5, 1, 2, 6])

let d = { dx: 0, dy: 1 }
a.foo = 13

assert({ ea: 7, ...d, eb: 9 }, { ea: 7, dx: 0, dy: 1, eb: 9 })
assert({ fa: 7, ...d, fb: 9, dy: 10 }, { fa: 7, dx: 0, fb: 9, dy: 10 })
assert({ bar: 17, ...a }, { "0": 1, "1": 2, bar: 17, foo: 13 })
