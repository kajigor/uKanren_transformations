filter (static dynamic) 
 evalo fm r = (fm == Num r | (fresh x, y, xr, yr in ((fm == Sum x y & Unfold evalo x xr & Unfold evalo y yr & Unfold addo xr yr r))));
filter (static dynamic dynamic)
 addo x y z = ((x == Zero & z == y) | (fresh x' in ((x == Succ x' & Unfold addo x' (Succ y) z))));
filter ()
 fail  = Memo fail [];

(fresh a, b in (Unfold evalo (Sum (Num Zero) (Sum (Num a) (Sum (Num (Succ Zero)) (Num b)))) (Succ (Succ (Succ Zero)))))