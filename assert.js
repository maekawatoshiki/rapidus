module.exports.deepStrictEqual = function(x, y) {
  console.log('assert_seq ' + x + ' ' + y)
  if (!__assert_deep_seq(x, y)) throw new Error('assert failed.')
}
