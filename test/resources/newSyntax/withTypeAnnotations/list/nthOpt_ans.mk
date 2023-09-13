filter (dynamic static dynamic)
 nthOpt xs n r = (fresh h, t, x in (((xs == [] & r == None) | (xs == (h :: t) & ((n == Zero & r == Some h) | (n == Succ x & Unfold nthOpt t x r))))));
filter ()
 fail  = Memo fail [];

(fresh xs, n, r in (Memo nthOpt xs n r))