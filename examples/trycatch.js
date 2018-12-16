function tcf() {
    for (i = 0; i < 7; i++) {
        try {
            console.log("count is " + i)
            console.log("try_before")
            if (i >= 2) throw "thrown in try: count is " + i
            console.log("try_after")
        } catch(e){
            console.log("catch_before")
            if (i === 3) throw "thrown in catch: count is " + i
            console.log("catch_after")
        } finally {
            console.log("finally_before")
            if (i === 4) throw "thrown in finally: count is " + i
            console.log("finally_after")
        }
    }
}
try {
    tcf()
} 
catch
(e) 
{
    console.log("ERROR!")
}
console.log("finished: count is " + i)