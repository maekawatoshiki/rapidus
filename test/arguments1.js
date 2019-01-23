var a = []
function f (x, y, ...z) {
  a.push([arguments[0],arguments[1],arguments[2],arguments[3],arguments.length])
  a.push([x, y, z])
}

f(1, 2, 3, 4)
f(5, 6, 7)
f(8, 9)
f(10)

console.log(a)
a
