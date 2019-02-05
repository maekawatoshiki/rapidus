const i = 100
let j = 0
for (let i = 0; i < 5; i++) {
  console.log(1)
  const i = 1000
  j = 10 * i
}
console.log(i, j)

while(true){
  j--
  if(j==0)break
}