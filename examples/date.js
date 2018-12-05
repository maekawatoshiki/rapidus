var start = Date.now();

function ack(m, n) {
  if (m == 0) {
    return n + 1;
  } else if (n == 0) {
    return ack(m - 1, 1);
  } else {
    return ack(m - 1, ack(m, n - 1));
  }
}

ack(3, 10);

var end = Date.now();

console.log("elapsed time: " + ((end - start) / 1000) + "s")
