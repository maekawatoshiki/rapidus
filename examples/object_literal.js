var a = 123;
var obj = {
  a,
  hello: "world",
  3.14: 1592,
  show_hello: function () { console.log("this.hello =", this.hello) }
};
console.log( obj.hello )
console.log( obj.a )
obj.show_hello()
