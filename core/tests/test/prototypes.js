var assert = require('./tests/assert').deepStrictEqual

assert(Object.prototype.constructor === Object, true)
assert(Function.prototype.constructor === Function, true)
assert(Array.prototype.constructor === Array, true)
assert(Error.prototype.constructor === Error, true)
//assert(Date.prototype.constructor === Date)

assert(Object.prototype.__proto__ === null, true)
assert(Function.prototype.__proto__ === Object.prototype, true)
assert(Array.prototype.__proto__ === Object.prototype, true)
assert(Error.prototype.__proto__ === Object.prototype, true)
//assert(Date.prototype.__proto__ === Object.prototype)

assert(Object.__proto__ === Function.prototype, true)
assert(Function.__proto__ === Function.prototype, true)
assert(Array.__proto__ === Function.prototype, true)
assert(Error.__proto__ === Function.prototype, true)
//assert(Date.__proto__ === Function.prototype)

var object = new Object()
assert(object.__proto__ === Object.prototype, true)
var object = new Function()
assert(object.__proto__ === Function.prototype, true)
var object = new Array()
assert(object.__proto__ === Array.prototype, true)
var object = new Error()
assert(object.__proto__ === Error.prototype, true)
//var object = new Date()
//assert(object.__proto__ === Date.prototype)