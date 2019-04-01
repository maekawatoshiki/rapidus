var a = []

a.push(Object.prototype.constructor === Object)
a.push(Function.prototype.constructor === Function)
a.push(Array.prototype.constructor === Array)
//a.push(Error.prototype.constructor === Error)
//a.push(Date.prototype.constructor === Date)

a.push(Object.prototype.__proto__ === null)
a.push(Function.prototype.__proto__ === Object.prototype)
a.push(Array.prototype.__proto__ === Object.prototype)
//a.push(Error.prototype.__proto__ === Object.prototype)
//a.push(Date.prototype.__proto__ === Object.prototype)

a.push(Object.__proto__ === Function.prototype)
a.push(Function.__proto__ === Function.prototype)
a.push(Array.__proto__ === Function.prototype)
//a.push(Error.__proto__ === Function.prototype)
//a.push(Date.__proto__ === Function.prototype)

var object = new Object()
a.push(object.__proto__ === Object.prototype)
//var object = new Function()
//a.push(object.__proto__ === Function.prototype)
//var object = new Array()
//a.push(object.__proto__ === Array.prototype)
//var object = new Error()
//a.push(object.__proto__ === Error.prototype)
//var object = new Date()
//a.push(object.__proto__ === Date.prototype)

console.log(a)
a
