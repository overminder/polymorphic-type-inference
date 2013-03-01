fibo n = if n < 2 then n else fibo (n - 1) + fibo (n - 2);

main = fibo 10;

