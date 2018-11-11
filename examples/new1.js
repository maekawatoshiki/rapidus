function NumMaker(init_val) {
  this.value = init_val
  this.increment = function() { this.value = this.value + 1 }
  return 123
}

console.log("NumMaker(10) =", NumMaker(10))
var nm = new NumMaker(10)
console.log("nm = new NumMaker(10)\nnm.value =", nm.value)
nm.increment()
console.log("nm.increment()\nnm.value =", nm.value)
