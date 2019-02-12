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

label1: // -> this label is invalid in node.js.
var a = 0
for (let i = 0; i < 10; i++) {
  break label2 // -> error in node.js
}