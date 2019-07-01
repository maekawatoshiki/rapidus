let assert = require('assert').deepStrictEqual

function Product(name, price) {
  this.name = name
  this.price = price
}

function Food(name, price) {
  Product.call(this, name, price)
  this.category = 'food'
}

assert(new Food('cheese', 5).name, 'cheese')
// expected output: "cheese"
