/* eslint-disable no-console */
let array = []
for (let i = 0; i < 1000; i++) {
  array.push({ index: i })
}
console.log(array.prototype)
array.map(e => {
  e.double = e.index * 2
})
console.log(array[500])
