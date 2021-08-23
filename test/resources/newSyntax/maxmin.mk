le x y b = 
  (x == Zero & b == trueo) |
  ( fresh z in 
      x == Succ z & y == Zero & b == falso 
  ) | 
  ( fresh x', y' in 
      x == Succ x' & y == Succ y' & le x' y' b
  );

gt x y b = 
  (x == Zero & b == falso) | 
  (fresh z in 
      x == Succ z & y == Zero & b == trueo
  ) | 
  (fresh x', y' in 
      x == Succ x' & y == Succ y' & gt x' y' b
  );

maxmin x a i = 
  (x == [] & a == Zero & i == Zero) | 
  (fresh h, t in 
      x == (h :: t) & 
      max t h a & 
      min t h i 
  );

max x n m = 
  (x == [] & m == n) |
  (fresh h, t in 
      x == (h :: t) & 
      ( (le h n trueo & max t n m) | 
        (gt h n trueo & max t h m)
      ) 
  );

min x n m = 
  (x == [] & m == n) |
  (fresh h, t in 
      x == (h :: t) & 
      ( (le h n trueo & min t h m) | 
        (gt h n trueo & min t n m)
      ) 
  );


? maxmin x a i
