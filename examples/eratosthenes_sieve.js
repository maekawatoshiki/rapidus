let sieve = [], max = 1000
sieve.push(false)
for (let i = 1; i < max; ++i)
  sieve.push(true)
for (let i = 0; i * i < max; i++) 
  if (sieve[i])
    for (let k = i + 1; (i + 1) * k <= max; k++)
      sieve[(i + 1) * k - 1] = false;
for (let i = 0; i < max; i++) 
  if (sieve[i]) console.log(i + 1);
