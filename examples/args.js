function f(x, y, ...z) {
  console.log(x, y, z, arguments[0], arguments[1], arguments[2], arguments[3])
}

f(1)
f(1, 2)
f(1, 2, 3)
f(1, 2, 3, 4)
