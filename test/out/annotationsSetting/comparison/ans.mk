filter (dynamic static dynamic) 
 gto x y b = ((x == Zero & b == Falso) | (fresh z in ((x == Succ z & y == Zero & b == Trueo))) | (fresh x', y' in ((x == Succ x' & y == Succ y' & Unfold gto x' y' b))));
filter (dynamic static dynamic) 
 leo x y b = ((x == Zero & b == Trueo) | (fresh z in ((x == Succ z & y == Zero & b == Falso))) | (fresh x', y' in ((x == Succ x' & y == Succ y' & Unfold leo x' y' b))));
filter () 
 fail  = Memo fail [];

(fresh a, b in ((Memo leo a b True & Memo gto b a True)))