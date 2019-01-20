module.exports = {
  deepEqual: function(x, y) {
    // JIT in module cause LLVM error.
    console.log("assert_seq " + x + " " + y)
    __assert(x, y)
  }
}