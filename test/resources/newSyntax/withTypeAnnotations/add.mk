filter (dynamic static dynamic)
addo x y z = 
  (x == Zero & z == y) | 
  ( fresh x' in 
     x == Succ x' & addo x' (Succ y) z
  );

? addo x y z