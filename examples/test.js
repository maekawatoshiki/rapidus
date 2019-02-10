var a = 0
function f() {
  //let a = 0
  {
    while (true) {
      var a = 0
      var a = 3
      break
    }
  }
}
f()
