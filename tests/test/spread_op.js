let assert = require('assert').deepStrictEqual

let a = [1, 2]
let b = [4, 5]
let c = { a, b }

assert([0, ...a, 3], [0, 1, 2, 3])
assert([...a, 3], [1, 2, 3])
assert([0, ...a], [0, 1, 2])
assert([0, ...a, ...b, 6], [0, 1, 2, 4, 5, 6])
assert([0, ...c.b, ...c.a, 6], [0, 4, 5, 1, 2, 6])