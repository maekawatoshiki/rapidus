module.exports.deepStrictEqual = function(x, y) {
  console.log('assert_seq ' + x + ' ' + y)
  //57 + x(100)
  if (!__assert_deep_seq(x, y)) throw 100
}
