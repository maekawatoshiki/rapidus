let mod = require('./test/test_module.js')
mod.countup(5)
if (mod.count() !== 15) throw new Error()
