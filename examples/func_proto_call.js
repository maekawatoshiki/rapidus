function A(x, y) {
  this.x = x
  this.y = y
}

function B() {
  A.call(this, 1, 2)
}
/*
function C() {
  A.apply(this, [3, 4])
}
*/
var b = new B()
//var c = new C()
console.log(b.x, b.y)
//console.log(c.x, c.y)
