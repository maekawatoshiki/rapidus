let assert = (n, l, r) => {
  if (l !== r) {
    throw n
  }
}
let o = {
  h: function(str) {
    return str2 => {
      return str + str2
    }
  }
}
assert(0, new f().g(), 'OK')
assert(1, new o.h('O')('K'), 'OK')
function f() {
  this.name = 'OK'
  this.g = function() {
    return this.name
  }
  function g() {
    return 'NG'
  }
}
