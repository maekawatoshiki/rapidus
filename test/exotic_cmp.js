let assert = (x, y) => { if (x !== y) throw "err" };

assert( null == 0, false )
assert( null > 0, false )
assert( null < 0, false )
assert( null <= 0, true )
assert( null >= 0, true )
