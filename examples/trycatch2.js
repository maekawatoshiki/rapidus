function f() {
  try {
    console.log(0);
    throw 'bogus';
  } catch(e) {
    console.log(1);
    return true; 
    console.log(2); 
  } finally {
    console.log(3);
    return false; 
    console.log(4);
  }
  console.log(5); 
}

console.log('>', f());
