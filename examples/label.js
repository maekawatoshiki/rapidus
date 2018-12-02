var i, j;

loop1: 
for (i = 0; i < 3; i++) {
  loop2:
  j = 0
  while (j < 3) { 
    if (i === 1 && j === 1) {
      continue loop1;
    }
    console.log("i =", i, ", j =", j);
    j++;
  }
}
