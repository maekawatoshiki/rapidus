function tcf(j) {
  for (var i = 0; i < 7; i++) {
    console.log("  i=" + i)
    try {
      if (i >= 1) {
        var str = "thrown in try: i=" + i
        console.log("   ", str)
        throw str
      }
      console.log("    try(inner)")
    } catch (e) {
      if (i == 2 && j == 0) {
        var str = "thrown in catch: i=" + i
        console.log("   ", str)
        throw str
      }
      console.log("    catch(inner)")
    } finally {
      if (i == 3) {
        var str = "thrown in finally: i=" + i
        console.log("   ", str)
        throw str
      }
      console.log("    finally(inner)")
    }
  }
}

for (var j = 0; j < 2; j++) {
  console.log("j=" + j)
  try {
    tcf(j)
    console.log("try(outer)")
  } catch (e) {
    console.log("catch(outer)")
  }
}

console.log("finished: i=" + i)
