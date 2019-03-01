function f(x) {
  log(x);
  f(x+1);
}

a = 1;
f(a);
log(2);
