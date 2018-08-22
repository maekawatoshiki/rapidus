function sum() {
  var i = 0, sum = 0;
  while (i < arguments.length) {
    sum += arguments[i];
    i += 1;
  }
  return sum;
}

console.log( sum(1, 2, 3) )
