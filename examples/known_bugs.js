var a = 5
a += ' ' + undefined
console.log(a) // -> NaN in rapidus

var f = function() {
  this.name = 'ok'
  this.g = function() {
    console.log(this.name)
  }
}

new f().g() // -> error in rapidus
