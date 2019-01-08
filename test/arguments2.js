var a = []

function g(x, y, z) {
  a.push(arguments[0], arguments[1], arguments[2], arguments[3], y)
  arguments[1] = 'OK'
  a.push(arguments[0], arguments[1], arguments[2], arguments[3], y)
  y = 'NG'
  a.push(arguments[0], arguments[1], arguments[2], arguments[3], y)
}

g(10, 15, 20, 25)

console.log(a)
a
