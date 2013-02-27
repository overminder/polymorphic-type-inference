main = letrec double x = x + x;
              compose f g x = f (g x);
           in compose double double double;

