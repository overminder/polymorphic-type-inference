main = let addOne x = x + 1;
           double x = x + x;
           compose f g x = f (g x);
        in let twice f = compose f f;
            in twice addOne;

