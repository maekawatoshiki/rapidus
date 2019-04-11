const assert = (n, l, r) => {
  if (l !== r) {
    throw n
  }
}

const o = {
  f: function(str) {
    return str2 => {
      return str + str2
    }
  }
}

function f() {
  this.name = 'OK'
  this.g = function() {
    return this.name
  }
  function g() {
    return 'NG'
  }
}

let g = function(x) {
  return function(y) {
    return { ans: x * y }
  }
}

assert(0, new f().g(), 'OK')
assert(1, new o.f('O')('K'), 'OK')
assert(2, new new g(5)(4).ans, 20)
