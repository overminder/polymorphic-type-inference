main = let f0 x = (x, x); in
       let f1 y = f0 (f0 y); in
       let f2 y = f1 (f1 y); in
       let f3 y = f2 (f2 y); in
       --let f4 y = f3 (f3 y); in
       --let f5 y = f4 (f4 y); in
       let id z = z; in
       f3 id;

          
