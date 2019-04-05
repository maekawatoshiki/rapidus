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
  x: 1, y: 2, sum: function () { console.log("x + y =", this.x + this.y) }
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
// // let v = new Vector(1, 2) // // console.log(v.norm2())
v = new Vector2(1, 2)
console.log(v.norm2())

xxx = 1;
console.log("xxx =", xxx)
console.log("this.xxx =", this.xxx);

console.log((function(){return this.xxx})());

let expr = "3*2+54"
let pos = 0

calc()

function calc() {
  function eval_node(node) {
    let op = node.op;
    if (op == "num") return parseFloat(node.num);
    else {
      let left  = eval_node(node.left);
      let right = eval_node(node.right);
      if      (op == "+") return left + right;
      else if (op == "*") return left * right;
    }
  }
  let node = expr_add_sub();
  console.log("expr:", expr);
  console.log("node:", node);
  console.log("answer:", eval_node(node))
}

function expr_add_sub() {
  let left = expr_mul_div();
  while (pos < expr.length && expr[pos] == "+") {
    let op = expr[pos++];
    let right = expr_mul_div();
    left = {op, left, right};
  }
  return left;
}

function expr_mul_div() {
  let left = expr_number();
  while (pos < expr.length && expr[pos] == "*") {
    let op = expr[pos++];
    let right = expr_number();
    left = {op, left, right};
  }
  return left;
}

function expr_number() {
  let num = ""; 
  while (pos < expr.length && 
         "0123456789".indexOf(expr[pos]) != -1) {
    let c = expr[pos++];
    num += c; 
  }
  return {op: "num", num};
}

a = 123;
for (let a = 0; a < 10; a++) {
  console.log(a);
  if (a == 5) break;
}
console.log(a)

console.log(typeof undefined)
console.log(typeof null)
console.log(typeof 1.2)
console.log(typeof false)
console.log(typeof "hello")
console.log(typeof fact)

function A(x, y) { this.x = x; this.y = y }
function B() { A.call(this, 1, 2) }
let b = new B()
console.log(b.x, b.y)
console.log(this.x, this.y)

function f(x, y) {
  console.log(x, y)
}

f()
f(1)
f(1,2)

let ary = [1, , , 2]
console.log(ary)
ary.x = 123
console.log(ary)
console.log(ary.length)
ary.length = 100
console.log(ary)

let temperature = {
  total: 0, count: 0,
  set current(t) {
    this.total += t;
    this.count++;
  },
  get average() {
    return this.total / this.count;
  },
};

console.log(temperature)
temperature.current = 19
temperature.current = 25
temperature.current = 15
temperature.current = 34
console.log( temperature.average )

ary = []
ary[3] = 123
console.log(ary)
ary.length=1
console.log(ary[30])

let color = {
  red:   Symbol(),
  green: Symbol(),
  blue:  Symbol()
}

let objects = {}
objects[color.red]   = ["apple"]
objects[color.green] = ["leaf"]
objects[color.blue]  = ["sky"]
objects[color.red].push("tomato")
console.log(objects[color.red])
console.log(objects[color.green])
console.log(objects[color.blue])

let xx = Symbol.for("xx")
let xx2= Symbol.for("xx")
console.log(xx === xx2)
console.log(Symbol.keyFor(xx))
