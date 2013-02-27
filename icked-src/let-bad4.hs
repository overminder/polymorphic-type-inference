-- oops

main = let s f g x = f x (g x);
           k x y = x;
        in let oops x = s k k x x;
            in oops;

