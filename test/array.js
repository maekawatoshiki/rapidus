var a = [1, 2, 'three', 4]
var b = []
function A(x, y) {
  this.x = x * 2
  this.y = x + y
}
function B() {
  A.call(this, 1, 2)
}
function C() {
  A.apply(this, [3, 4])
}
var bf = new B()
var cf = new C()
b.push(bf.x, bf.y)
b.push(cf.x, cf.y)
b.push(new Array(3,4))
b.push(
  a.map(function(x) {
    return x + 1
  })
)
b.push(a.pop())
b.push(a)
console.log(b.toString())
b.toString()
