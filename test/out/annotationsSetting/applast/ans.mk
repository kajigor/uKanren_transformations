filter (dynamic dynamic dynamic) 
 appendo xs ys rs = ((xs == [] & rs == ys) | (fresh h, t, ts in ((xs == (h :: t) & rs == (h :: ts) & Memo appendo t ys ts))));
filter (dynamic dynamic) 
 lasto x ys = (fresh h, t in ((ys == [x] | (ys == (h :: t) & Memo lasto x t))));
filter (dynamic dynamic dynamic) 
 applasto l x lst = (fresh lx in ((Unfold appendo l [x] lx & Unfold lasto lst lx)));
filter () 
 fail  = Memo fail [];

(fresh l, x, lst in (Unfold applasto l x lst))