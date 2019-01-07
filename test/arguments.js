var a = []
function f (x, y, ...z) {
  a.push(arguments)
  a.push(x, y, z)
}

f(1, 2, 3, 4)
f(5, 6, 7)
f(8, 9)
f(10)

function g (x, y, z) {
  a.push(arguments.toString(), y)
  arguments[1] = 'OK'
  /*
  a.push(arguments, y)
  y = 'OKOK'
  a.push(arguments, y)
  */
}

g(10, 15, 20, 25)

console.log(a)
a
