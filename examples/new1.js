function NumMaker(init_val) {
  this.value = init_val
  this.increment = function() { this.value = this.value + 1 }
  return 123
}

console.log("NumMaker(1) =", NumMaker(1))
var nm = new NumMaker(10)
console.log(nm.value)
nm.increment()
console.log(nm.value)
