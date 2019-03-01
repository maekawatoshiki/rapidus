function f(x) {
  var a = x;
  {
    let a = x + x;
    log(a);
  }
  log(a);
}

a = 2;
f(a);
