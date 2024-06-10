filter (dynamic dynamic dynamic dynamic) 
 double_apppendo x y z res = (fresh t in ((Unfold appendo x y t & Unfold appendo t z res)));
filter (dynamic dynamic dynamic) 
 appendo x y xy = ((x == [] & y == xy) | (fresh h, t, ty in ((x == (h :: t) & xy == (h :: ty) & Memo appendo t y ty))));
filter () 
 fail  = Memo fail [];

(fresh x, y, z, res in (Unfold double_apppendo x y z res))