filter ()
 fail  = Memo fail [];
filter (dynamic static static)
 rev x acc y = ((x == [] & acc == y) | (fresh h, t in ((x == (h :: t) & Unfold rev t ((h :: acc)) y))));

(fresh x in (Unfold rev x [] ((A :: (B :: (C :: [D]))))))