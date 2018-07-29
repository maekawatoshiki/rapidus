function AAA() {
  var a = 123;
  var f = function () { return a; }
  var g = function () { return f(); }
  a = 456
  return g
}

console.log( AAA()() )

var a = 789;
function f() {
  console.log(a)
  a = 123;
  b = 123;
}

f();
console.log(a)
console.log(b)
