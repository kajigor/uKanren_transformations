filter (static dynamic)
 my_clause clause body =
  ((fresh x, y, z, r, i in
    ((clause == DoubleApp x y z r &
      body == (App x y i :: [App i z r])))) |
   (fresh l in
    ((clause == App [] l l & body == []))) |
   (fresh h, x, y, z in
    ((clause == App (h :: x) y (h :: z) & body == [App x y z]))) |
    body == [clause] |
   (clause == Solve2 [] & body == []) |
   (fresh a, t in
    ((clause == Solve2 (a :: t) & body == (Solve_atom2 a :: [Solve2 t])))) |
   (fresh a, b in
    ((clause == Solve_atom2 a & body == (My_clause2 a b :: [Solve2 b])))) |
   (fresh l in
    ((clause == My_clause2 App [] l l [] & body == []))) |
   (fresh h, x, y, z in
    ((clause == My_clause2 App (h :: x) y (h :: z) [App x y z] & body == []))));

filter (static)
 solve_atom a = (fresh b in ((Unfold my_clause a b & Memo solve b)));
filter (static)
 solve stmts = (stmts == [] | (fresh a, t in ((stmts == (a :: t) & Unfold solve_atom a & Unfold solve t))));
filter ()
 fail  = Memo fail [];

(fresh x, y, z in (Unfold solve [DoubleApp x y z ([S, S O, S (S O), S O])]))