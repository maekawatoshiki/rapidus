var i, j;
var ans = []
loop1: 
for (i = 0; i < 3; i++) {
  loop2:
  j = 0
  while (j < 3) { 
    if (i === 1 && j === 1) {
      continue loop1;
    }
    ans.push(i, j);
    j++;
  }
}
console.log(ans)
ans
