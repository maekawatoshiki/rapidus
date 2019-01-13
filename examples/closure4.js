var a = 0
function f() {
  var b = 0
  return {
    a: a,
    inc_b: function() {
      b++
    },
    inc_a: function() {
      a++
    },
    print_b: function() {
      console.log(b)
    }
  }
}
g = f()
console.log(g.a)
a = 100
console.log(g.a)

g.inc_a()
g.inc_b()
g.print_b()
console.log(a)
console.log(g.a)
console.log(f().a)
f().print_b()
