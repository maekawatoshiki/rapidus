let assert = require('./tests/assert').deepStrictEqual
// Simple assert ever

assert([] == 0, true)
assert('0' == 0, true)
assert('\t' == 0, true)
assert([] != '0', true)
assert('0' != '\t', true)
assert('\t' != [], true)
