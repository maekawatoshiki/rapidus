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
