label1: // -> this label is invalid in node.js.
var a = 0
for (let i = 0; i < 10; i++) {
  break label2 // -> error in node.js
}
