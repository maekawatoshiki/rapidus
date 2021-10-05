let assert = (x) => { if (!x) throw "err" }

let color = {
  red:   Symbol(),
  green: Symbol(),
  blue:  Symbol()
}

let objects = {}
objects[color.red]   = ["apple"]
objects[color.green] = ["leaf"]
objects[color.blue]  = ["sky"]
objects[color.red].push("tomato")
assert(objects[color.red][0] == "apple" && objects[color.red][1] == "tomato")
assert(objects[color.green][0] == "leaf")
assert(objects[color.blue][0] == "sky")

let xx = Symbol.for("xx")
let xx2 = Symbol.for("xx")
assert(xx === xx2)
assert(Symbol.keyFor(xx) == "xx")
