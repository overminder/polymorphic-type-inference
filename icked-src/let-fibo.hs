main = let fibo fibo n = if n < 2 then n else fibo (n - 1) + fibo (n - 2); in
       let fibo = fix fibo; in
       fibo 10;
