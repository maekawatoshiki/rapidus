function f() {
  try {
    console.log(0);
    try {
      throw 123
    } catch(e) {
      return 1;
    } finally {}
    throw 'bogus';
  } catch(e) {
    console.log(1);
  } finally {
    console.log(2); 
  }
  console.log(5); 
}

try {
console.log('>', f());
  throw 10110
} catch (e){
  console.log(e)
}
