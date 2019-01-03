var a = 5
a += ' ' + undefined
console.log(a) // -> NaN in rapidus

var f = function() {
  this.name = 'ok'
  this.g = function() {
    console.log(this.name)
  }
  //function g() { return 'NG' }
}

new f().g() // -> error in rapidus (parsed as "new (f().g())" )

function c (a, b, c) {
  try {
    console.log(arguments, [a, b, c])
    throw 100
  } catch (e) {
    console.log(arguments, [a, b, c])
  }
}

c(1, 2, 3) // -> can not access arguments in catch clause.