function A(x, y) {
  this.x = x;
  this.y = y;
}

function B() {
  A.call(this, 1, 2);
}

var b = new B();
console.log(b.x, b.y);
