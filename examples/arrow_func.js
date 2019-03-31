this.a = 50
let f = () => {
  console.log(this.a)
}
let g = function() {
  console.log(this.a)
}
f() // 50
g() // undefined
let obj = { a: 70, f: f, g: g }
obj.f() // 50
obj.g() // 70
