let assert = require('assert').deepStrictEqual

var a = [1, 2, 'three', 4]
assert(a[2], 'three')
a.push(5, 6, 7)

var b = a.map(elem => elem + 1)
assert(b[5], 7)
assert(b[2], 'three1')
b.length = 2
assert(b, [2, 3])
b.length = 4
assert(b.length, 4)

assert(new Array(), [])
assert(new Array(1, 'd', { a: 9 }), [1, 'd', { a: 9 }])
assert(new Array(3).length, 3)

let c = [1, 2]
c[2] = 3
assert(c.join(), '1,2,3')
assert(c.join(undefined), '1,2,3')
assert(c.join('_'), '1_2_3')
assert(c.join(2), '12223')
