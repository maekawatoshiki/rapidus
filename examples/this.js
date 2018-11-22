count = 100
var c = {
  count: 0,
  up: function() {
    console.log(++this.count)
  }
}

var adder = c.up

var a = {
  count: 123,
  up: adder,
}

c.up()  // 1
adder()  // 101
a.up() // 124
