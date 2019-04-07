let assert = (x) => { if (!x) throw "err" }

var a = [1, 2, 'three', 4]
assert(a[2] == 'three')
a.push(5, 6, 7)

var b = a.map((elem) => elem + 1)
assert(b[5] == 7)
assert(b[2] == 'three1')
