
solve stmts =
  stmts == Nil |
  (fresh a, t in
    stmts == (a :: t) &
    solve_atom a &
    solve t
  );

solve_atom a =
  fresh b in
     my_clause a b &
     solve b;

my_clause clause body =
  (
    fresh x, y, z, r, i in
      clause == DoubleApp x y z r &
      body == [App x y i, App i z r]
  ) |
  (
    fresh l in
      clause == App (Nil) l l &
      body == Nil
  ) |
  (
    fresh h, x, y, z in
      clause == App (h :: x) y (h :: z) &
      body == [App x y z]
  );

test2 r =
  solve_atom (Solve_atom2 (App ([O, S O, S (S O)]) ([O, S O, S (S O), S (S (S O))]) r));

test1 r =
  solve_atom (App ([O, S O, S (S O)]) ([O, S O, S (S O), S (S (S O))]) r);

help x y z = 
	solve [DoubleApp x y z ([S (S O), S O, S (S O), S O])];

? help x y z