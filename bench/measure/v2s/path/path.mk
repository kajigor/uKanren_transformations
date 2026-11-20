eq_nat x y r = 
  (x == Zero & y == Zero & r == Trueo) | 
  ( fresh x1 in 
      (x == Succ x1 & y == Zero & r == Falso)) | 
  ( fresh y1 in 
      (x == Zero & y == Succ y1 & r == Falso)) | 
  ( fresh x1, y1 in  
      (x == Succ x1 & y == Succ y1 & eq_nat x1 y1 r)); 

eq_pair a b r = 
  fresh r2, r1, b2, b1, a2, a1 in  
    a == Pair a1 a2 & 
    b == Pair b1 b2 & 
    eq_nat a1 b1 r1 & 
    eq_nat a2 b2 r2 & 
    ( r1 == Falso & r == Falso | 
      r1 == Trueo & r == r2); 
  
elem x g r = 
  ( g == [] & (r == Falso)) | 
  ( fresh r1, ys, y in 
      g == (y :: ys) &
      eq_pair x y r1 & 
      ( r1 == Trueo & r == Trueo |  
        r1 == Falso & elem x ys r )); 
  
is_path c g r =
  ( c == [] & r == Trueo ) |
  ( fresh q2 in 
      c == [q2] & r == Trueo ) | 
  ( fresh q5, q4, xs, x2, x1 in 
      c == (x1 :: (x2 :: xs)) & 
      elem (Pair x1 x2) g q4 & 
      is_path (x2 :: xs) g q5 & 
      ( q4 == Falso & r == Falso | 
        q4 == Trueo & r == q5 ) 
  );

? is_path q [Pair Zero (Succ Zero), Pair (Succ Zero) (Succ (Succ Zero)), Pair (Succ (Succ Zero)) Zero] Trueo