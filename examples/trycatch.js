function tcf(j) {
  for (var i = 0; i < 7; i++) {
    console.log("  i=" + i)
    var that = this
    try {
      if (i >= 1) {
        var str = "thrown in try: i=" + i
        console.log("   ", str)
        throw str
      }
      console.log("    try(inner)      arg[0]=", arguments[0], this === that)
    } catch (ex) {
      console.log("    error obj=", ex)
      if (i == 2 && j == 0) {
        var str = "thrown in catch: i=" + i
        console.log("   ", str)
        throw str
      }
      console.log("    (in catch) i:", i, "j:", j)
      console.log("    catch(inner)    arg[0]=", arguments[0], this === that)
      //arguments[0] = 100
    } finally {
      if (i == 3) {
        var str = "thrown in finally: i=" + i
        console.log("   ", str)
        throw str
      }
      console.log("    finally(inner)  arg[0]=", arguments[0], this === that)
    }
  }
}

for (var j = 0; j < 2; j++) {
  console.log("j=" + j)
  var that = this;
  try {
    tcf(j)
    console.log("try(outer)", that === this)
  } catch (e) {
    console.log("catch(outer) error obj=", e, that === this)
  }
}
