let assert = (x, y) => {
  if (x !== y) throw 100
}
assert(__assert_deep_seq(1, 1), true)
assert(__assert_deep_seq(1, 2), false)
assert(__assert_deep_seq(1, '1'), false)
assert(__assert_deep_seq(1, [1]), false)
assert(__assert_deep_seq(1, { a: 1 }), false)
assert(__assert_deep_seq([1, 2, 3], [1, 2, 3]), true)
assert(__assert_deep_seq([1, 2, 3], [1, 2, 3, 4]), false)
assert(__assert_deep_seq([1, 2, 3], [2, 3, 1]), false)
assert(__assert_deep_seq({ a: 'foo', b: 5 }, { b: 5, a: 'foo' }), true)
assert(__assert_deep_seq({ a: 'foo', b: 5 }, { c: 5, a: 'foo' }), false)
assert(
  __assert_deep_seq(
    { a: 'foo', b: 5, c: [0, 1, { d: 'foo' }] },
    { c: [0, 1, { d: 'foo' }], b: 5, a: 'foo' }
  ),
  true
)
