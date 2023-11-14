filter (static static dynamic)
 sum1 lst s1 s2 = ((lst == [] & s1 == s2) | (fresh h, t, s0 in ((lst == (h :: t) & Unfold add h s1 s0 & Unfold sum1 t s0 s2))));
filter (static dynamic)
 sum ns s = Unfold sum1 ns O s;
filter (static dynamic)
 squares ns sons = ((ns == [] & sons == []) | (fresh h1, t1, h2, t2 in ((ns == (h1 :: t1) & sons == (h2 :: t2) & Unfold square h1 h2 & Unfold squares t1 t2))));
filter (static dynamic)
 square ns son = Unfold multiply ns ns son;
filter (static static dynamic)
 upto m n lst = ((m == S n & lst == []) | (Unfold le m n & (fresh t in ((Memo upto (S m) n t & lst == (m :: t))))));
filter (static static)
 le m n = (m == O | (fresh t, t1 in ((m == S t & n == S t1 & Unfold le t t1))));
filter (static dynamic)
 sumsquaresupto n s = (fresh ns, sons in ((Unfold upto (S O) n ns & Unfold squares ns sons & Unfold sum sons s)));
filter (static static dynamic)
 multiply x y z = ((y == O & z == O) | (fresh t, z1 in ((y == S t & Unfold add x z1 z & Unfold multiply x t z1))));
filter (static static dynamic)
 add x y z = ((x == O & y == z) | (fresh t1, t2 in ((x == S t1 & z == S t2 & Unfold add t1 y t2))));
filter ()
 fail  = Memo fail [];

(fresh s in (Unfold sumsquaresupto (S (S (S (S O)))) s))