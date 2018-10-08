var str = "いろはにほへとちりぬるを";
for (var k = 0; k < 1000; k += 1) 
  for (var i = 0; i < 12; i += 1) 
    process.stdout.write(str[i]);

console.log("\nlength of", str, "is", str.length);

console.log("あいうえお" * 10)
