function fibo(x) {
  if (x < 2) return 1
  return fibo(x - 1) + fibo(x - 2)
}

function do_fibo(x, sw) {
  var t = Date.now()
  var ans = fibo(x)
  console.log('JIT', sw, Date.now() - t, 'ms')
}

__enableJit(false)
do_fibo(25, 'OFF')
__enableJit(true)
do_fibo(25, 'ON ')
