var a = 0
try {
  new f()
} catch (e) {
  console.log(a)
}

function f() {
  var a = 100
  throw 'err'
}
