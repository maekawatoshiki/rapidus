let assert = cond => {
  if (!cond) throw 'err'
}

assert(typeof 1 == 'number')
assert(typeof 'string' == 'string')
assert(typeof {} == 'object')
assert(typeof [] == 'object')
assert(typeof (() => 0) == 'function')
assert(typeof function() {} == 'function')
assert(typeof Symbol() == 'symbol')
assert(typeof new Error('message') == 'error')
