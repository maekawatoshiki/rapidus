function a(x, y, ...z) {
  console.log(x, y, z)
}

a(1, 2, 3, 4, 5)
a(6, 7, 8, 9)
a(10, 11, 12)
a(13, 14)
a(15)

var count = 1000
var module = (function() {
  var count = 0
  return {
    increment: function() {
      count++
    },
    show: function() {
      console.log(count)
    }
  }
})()

module.show() // 0
module.increment()
module.show() // 1

console.log(count) // undefined
