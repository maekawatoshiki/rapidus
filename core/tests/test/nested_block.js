var assert = (x, y) => { if (x !== y) throw "err" }
const i = 100
try {
  const i = 10 
} catch (e) {
  const i = 20
} finally {
  const i = 30
  label:
  while(true) {
    const i = 1
    try {
      const i = 2
      try {
        const i = 3
        while(true) {
          const i = 4
          while(true) {
            const i = 5
            break label
          }
        }
      } catch (e) {}
    } catch(e) {}
  }
  assert(i, 30)
}
