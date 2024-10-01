leo x y b =
  (x == Zero & b == Trueo) |
  (fresh zz in
    (x == Succ zz & y == Zero & b == Falso)
  ) |
  (fresh x', y' in
    (x == Succ x' & y == Succ y' & leo x' y' b)
  );

gto x y b =
  (fresh zz in x == Succ zz & y == Zero & b == Trueo) |
  (x == Zero & b == Falso) |
  (fresh x', y' in x == Succ x' & y == Succ y' & gto x' y' b);

minmaxo a b min max = 
  (min == a & max == b & leo a b Trueo) | 
  (min == b & max == a & gto a b Trueo); 

smallesto l s l' = 
  (l ==  [s] & l' == []) | 
  (fresh h, t, s', t', max in 
    l' == (max :: t') & 
    l == (h :: t) & 
    minmaxo h s' s max & 
    smallesto t s' t'
  ); 

sorto x y = 
  ( x == [] & y == []) | 
  ( fresh s, xs, xs' in 
      y == (s :: xs') & 
      sorto xs xs' & 
      smallesto x s xs 
  ); 
  
? sorto [Succ Zero, Zero, Succ (Succ Zero), Succ Zero, Zero] reslst