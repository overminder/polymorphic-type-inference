main = let s f g x = f x (g x);
           k x y = x;
        in s k k 3;


