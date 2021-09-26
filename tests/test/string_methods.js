let assert = require('./tests/assert').deepStrictEqual
assert('thereisapencil'.split('e'), ['th', 'r', 'isap', 'ncil'])
assert('thereisapencil'.indexOf('pen'), 8)
