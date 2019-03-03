function sum () { console.log(this.x + this.y) }
var obj = {
  x: 1, y: 2, sum: sum 
};
obj.sum()

function fact(x) {
  if (x == 1) return 1;
  else return fact(x - 1) * x;
}

console.log("fact(5):", fact(5))
