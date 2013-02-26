-- oops
main = let s = \f g x -> f x (g x);
        in let k = \x y -> x;
            in \x -> s k k x x;
