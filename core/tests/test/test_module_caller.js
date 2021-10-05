let mod = require('./tests/test/test_module.js')
mod.countup(5)
if (mod.count() !== 15) throw new Error()
