function Hello() {
  this.say = "hello"
  return this
}

hello = new Hello()
console.log( hello.say )
