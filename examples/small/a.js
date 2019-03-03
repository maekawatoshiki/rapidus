function sum () { console.log(this.x + this.y) }
var obj = {
  x: 1, y: 2, sum: sum 
};
obj.sum()
