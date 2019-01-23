var assert = require('assert').deepEqual

assert([] == 0, true)
assert('0' == 0, true)
assert('\t' == 0, true)
assert([] != '0', true)
assert('0' != '\t', true)
assert('\t' != [], true)


