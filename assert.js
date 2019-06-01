exports.s = {
  deepStrictEqual: function(x, y) {
    console.log('assert_seq ' + x + ' ' + y)
    __assert_deep_seq(x, y)
  }
}
