// Show a mandelbrot set.

function mandelbrot(c_x, c_y, n) {
  var x_n = 0, y_n = 0, x_n_1 = 0, y_n_1 = 0;
  var i = 0;
  while (i < n) {
    x_n_1 = x_n * x_n - (y_n * y_n) + c_x;
    y_n_1 = 2.0 * x_n * y_n + c_y;
    var t = x_n_1 * x_n_1 + y_n_1 * y_n_1;
    if (t > 4.0) {
      return t
    } else {
      x_n = x_n_1;
      y_n = y_n_1;
    }
    i += 1;
  }
  return 0
}

var x_max = 2, x_min = -2, y_max = 1, y_min = -1, dx = 0.03, dy = 0.045;
var y = y_max;
while (y > y_min) {
  var x = x_min;
  var str = ""
  while (x < x_max) {
    var t = mandelbrot(x, y, 300);
    if (t > 8) 
      process.stdout.write('#')
    else if (t > 6) 
      process.stdout.write('*')
    else if (t > 4) 
      process.stdout.write('.')
    else 
      process.stdout.write(' ')
    x += dx
  }
  console.log()
  y -= dy
}
