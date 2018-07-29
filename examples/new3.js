function AAA() {
  this.x = 123;
  this.y = 456;
  this.show = function () { console.log("x =", this.x, ", y =", this.y) };
  this.maker = function () { this.z = 789 }
}

var a = new AAA();
a.show();
var b = new a.maker()
console.log(b.z)
a.maker()
console.log(a.z)
