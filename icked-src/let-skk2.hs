main = let s f g x = f x (g x);
           k x y = x;
        in let myId = s k k;
            in myId myId 3;

