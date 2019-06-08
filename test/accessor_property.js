let assert = require('./assert.js').deepStrictEqual
let obj = {
  _x: 7,
  get x() {
    return this._x * 5
  },
  set x(val) {
    this._x = val * 2
  }
}

assert(obj.x, 35)
obj.x = 123
assert(obj.x, 1230)
