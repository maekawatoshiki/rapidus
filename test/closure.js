var assert = require('assert').deepEqual
count = 1000
var mod = (function() {
  var count = 0
  return {
    count: 50,
    increment: function() {
      count++
    },
    show: function() {
      return count
    },
    nest: function() {
      return this.count
    }
  }
})()
var f = mod.show
var g = mod.nest
assert(mod.show(), 0)
mod.increment()
assert(mod.show(), 1)
assert(mod.nest(), 50)
assert(f(), 1)
assert(g(), 1000)
assert(mod.nest(), 50)
