var ans = []
count = 1000
var module = (function() {
  var count = 0
  return {
    count: 50,
    increment: function() {
      count++
    },
    show: function() {
      ans.push(count, this.count)
    },
    nest: function() {
      return this.count
    }
  }
})()
var f = module.show
module.show() // 0
module.increment()
module.show() // 1
f()
ans.push(module.nest())
console.log(ans)
ans
