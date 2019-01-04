function a(x, y, ...z) {
  console.log(arguments)
  console.log(x, y, z)
}

a(1, 2, 3, 4)
a(5, 6, 7)
a(8, 9)
a(10)

function b(x, y, z) {
  console.log(arguments, y)
  arguments[1] = 'OK'
  console.log(arguments, y)
  y = 'OKOK'
  arguments['prop'] = 'something'
  console.log(arguments, y)
}

b(10, 15, 20, 25)

function c(a, b, c) {
  try {
    console.log(arguments, [a, b, c])
    throw 100
  } catch (e) {
    console.log(arguments, [a, b, c])
  }
}

c(1, 2, 3)
