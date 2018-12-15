
var a = 0
try {
    a = 3
} catch(e){
    a = 5
} finally {
    a = 7
}
console.log(a)

for (i = 0; i < 1000; i++){
    if (i = 8) throw "count is " + i
}
console.log("finished")