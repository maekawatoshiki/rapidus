// Simple assert ever
let assert = (cond) => {
  if (!cond) throw "err"
};

assert([] == 0)
assert('0' == 0)
assert('\t' == 0)
assert([] != '0')
assert('0' != '\t')
assert('\t' != [])
