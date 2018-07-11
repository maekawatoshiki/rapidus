function AA () {
  function B() {
    function C() {
      console.log(i, k)
      return i + 123
    }
    return C
  }
  var i = 10, k = 20;
  return B()
}

console.log(AA()())
