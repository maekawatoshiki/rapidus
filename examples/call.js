const x = 10
function a(x) {
  return {
    b: function(x) {
      console.log(":", x)
    }
  }
}
new a(x)
/*
New(
  Call(
    Identifier("a"),
    [Identifier("x")]
  )
)
*/
a.b(x)
/*
  Call(
    Member(
      Identifier("a"),
      "b"
    ),
    [Identifier("x")]
  ),
*/
a(x)
/*
  Call(
    Identifier("a"),
    [Identifier("x")]
  )
*/