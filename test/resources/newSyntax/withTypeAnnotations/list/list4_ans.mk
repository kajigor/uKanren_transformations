filter (static dynamic dynamic)
 maxMino x m l = (Unfold maxo x m & Unfold mino x l);
filter (static static dynamic)
 mino1 x n m = ((x == [] & m == n) | (fresh h, t, z in ((x == (h :: t) & Unfold leo h n Trueo & Unfold mino1 t h m))) | (fresh h, t, z in ((x == (h :: t) & Unfold gto h n Trueo & Unfold mino1 t n m))));
filter (static dynamic)
 mino x m = ((x == [] & m == Zero) | (fresh h, t in ((x == (h :: t) & Unfold mino1 t h m))));
filter (static static dynamic)
 maxo1 x n m = ((x == [] & m == n) | (fresh h, t, z in ((x == (h :: t) & Unfold leo h n Trueo & Unfold maxo1 t n m))) | (fresh h, t, z in ((x == (h :: t) & Unfold gto h n Trueo & Unfold maxo1 t h m))));
filter (static dynamic)
 maxo x m = Unfold maxo1 x Zero m;
filter (static static dynamic)
 gto x y b = ((x == Zero & b == Falso) | (fresh z in ((x == Succ z & y == Zero & b == Trueo))) | (fresh x', y' in ((x == Succ x' & y == Succ y' & Unfold gto x' y' b))));
filter (static static dynamic)
 leo x y b = ((x == Zero & b == Trueo) | (fresh z in ((x == Succ z & y == Zero & b == Falso))) | (fresh x', y' in ((x == Succ x' & y == Succ y' & Unfold leo x' y' b))));
filter ()
 fail  = Memo fail [];

(fresh x, m, l in (Memo maxMino x m l))