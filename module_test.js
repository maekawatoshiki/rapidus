let assert = require('assert.js')
let obj1 = { a: 1, b: 2 }
let obj2 = { a: 1, b: 3 }

assert.deepStrictEqual(obj1, obj2)
