function tcf() {
  try {
    try {
      console.log("try");
      throw 2
    } catch (e) {
      console.log("catch", e);
      throw 3;
    } finally {
      console.log("finally")
    }
  } catch (e) {
    console.log(e)
    throw 4
  }
  console.log("leave try-catch-finally")
}

try {
  console.log(tcf())
} catch (e) {
  console.log("catch", e)
}

var obj = {
  x: 1, y: 2, sum: function () { console.log(this.x + this.y) }
};
obj.sum()

function fact(x) {
  if (x == 1) return 1;
  else return fact(x - 1) * x;
}

console.log("fact(5):", fact(5))

function make_counter() {
  var n = 0;
  return function () { n = n + 1; return n; }
}

let counter1 = make_counter()
let counter2 = make_counter()
console.log(counter1())
console.log(counter1())
console.log(counter1())
console.log(counter2())
console.log(counter2())
console.log(counter2())

function f() {
  function g() {
    console.log(obj)
  }
  g()
}
f()

let a = 1;
while (a < 20) { 
  console.log(a);
  a *= 2;
}

function Vector(x, y) {
  return {
    x, 
    y, 
    norm2: function() { return this.x * this.x + 
                               this.y * this.y }
  }
}

function Vector2(x, y) {
  this.x = x
  this.y = y
  this.norm2 = function() { return this.x * this.x + 
                                   this.y * this.y }
}

let v = new Vector(1, 2)
console.log(v.norm2())
v = new Vector2(1, 2)
console.log(v.norm2())

xxx = 1;
console.log("xxx =", xxx)
console.log("this.xxx =", this.xxx);

console.log((function(){return this.xxx})());
