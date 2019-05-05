module.exports = {
  deepStrictEqual: function(x, y) {
    // JIT in module cause LLVM error.
    console.log('assert_seq ' + x + ' ' + y)
    __assert_deep_seq(x, y)
  }
}
