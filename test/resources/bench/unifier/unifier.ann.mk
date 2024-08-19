filter (static static dynamic)
eq_nat x y r = 
  ( x == Zero & y == Zero & r == Trueo ) |
  ( fresh x1 in 
      x == Succ x1 & y == Zero & r == Falso ) |
  ( fresh y1 in 
      x == Zero & y == Succ y1 & r == Falso ) |
  ( fresh x1, y1 in 
      x == Succ x1 & y == Succ y1 & Unfold eq_nat x1 y1 r ); 

filter (static dynamic static)
get_term var subst r = 
  ( subst == [] & r == None ) |
  ( fresh xs, x in  
      (subst == (x :: xs) & 
      ( ( var == Zero & x == r ) | 
        ( fresh n in  
            var == Succ n & Unfold get_term n xs r
        ))));

filter (dynamic static static static)
forall2 subst l1 l2 q0 = 
  ( l1 == [] & l2 == [] & q0 == Trueo ) |
  ( fresh q4, q3, ys, y, xs, x in  
      l1 == (x :: xs) & 
      l2 == (y :: ys) & 
      Unfold check_uni subst x y q3 & 
      Unfold forall2 subst xs ys q4 & 
      ( (q3 == Falso & q0 == Falso) | 
          (q3 == Trueo & q0 == q4))); 

filter (dynamic static static static)
check_uni subst t1 t2 r = 
  ( fresh q13, q12, a2, n2, a1, n1 in  
      t1 == Constr n1 a1 & 
      t2 == Constr n2 a2 & 
      Unfold eq_nat n1 n2 q12 & 
      Unfold forall2 subst a1 a2 q13 & 
      ( (q12 == Falso & r == Falso) | 
        (q12 == Trueo & r == q13))) |
  ( fresh q19, a, n, v in  
      t1 == Var_ v & 
      t2 == Constr n a & 
      Unfold get_term v subst q19 & 
      ( ( q19 == None & r == Falso) | 
        ( fresh t in  
            q19 == Some t & Unfold check_uni subst t t2 r ))) |
  ( fresh q22, v, a, n in  
      t1 == Constr n a & 
      t2 == Var_ v & 
      Unfold get_term v subst q22 & 
      ( ( q22 == None & r == Falso) | 
        ( fresh t in 
            (q22 == Some t & Unfold check_uni subst t1 t r)))) |
  ( fresh q25, v2, v1 in 
      t1 == Var_ v1 & 
      t2 == Var_ v2 & 
      Unfold get_term v1 subst q25 & 
      (( fresh t1' in
           q25 == Some t1' & Unfold check_uni subst t1' t2 r ) | 
       ( fresh q27 in  
          q25 == None & 
          Unfold get_term v2 subst q27 & 
          (( fresh q28 in q27 == Some q28 & r == Falso) | 
           ( q27 == None & Unfold eq_nat v1 v2 r))))); 

(fresh q in (Unfold check_uni q (Constr Zero [Var_ Zero, Constr (Succ Zero) []]) (Constr Zero [Constr (Succ (Succ Zero)) [], Var_ Zero]) Trueo))