var a = []
function f() {
  try {
    a.push(0);
    try {
      throw 123
    } catch(e) {
      return 1;
    } finally {}
    throw 'bogus';
  } catch(e) {
    a.push(1);
  } finally {
    a.push(2); 
  }
  a.push(5); 
}

try {
  a.push(f());
  throw 10110
} catch (e){
  a.push(e)
}

console.log(a)
a