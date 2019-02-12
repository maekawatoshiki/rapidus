var a = 10,
  b = 20
{
  let a = 100
  var b = 200
  console.log('block', a, b) // expect 100,200
}
console.log('global', a, b) // expect 10, 200

for (let a = 0; a < 4; a++) {
  let b = a * 2
}
console.log('let', a, b) // expect 10, 200

for (var a = 0; a < 4; a++) {
  var b = a * 2
}
console.log('var', a, b) // expect 4, 6

for (let a = 0; a < 4; a++) {
  let b = a * 3
  if (a % 2 == 0) {
    let b = a + 1
    console.log(a, b)
  }
  console.log(a, b)
}
console.log('nest', a, b) // expect 4, 6
