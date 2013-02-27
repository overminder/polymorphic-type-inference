main = let loop = let loop2 loop x = loop x;
                   in fix loop2;
        in loop;

