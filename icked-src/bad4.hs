-- oops
s f g x = f x (g x);
k x y = x;

oops x = s k k x x;

main = oops;
