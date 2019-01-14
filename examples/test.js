var a = []
for (var j = 0; j < 20; j++) {
  a = []
  f()
}

function f(){
  for (var i = 0; i < 20; i++) {
    a.push({ i: i, j: undefined })
  }
}
