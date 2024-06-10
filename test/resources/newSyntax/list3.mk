
reverso x y = ((x == [] & y == []) | (fresh h, t, rt in ((x == (h :: t) & reverso t rt & appendo rt [h] y))));

appendo x y xy = ((x == [] & y == xy) | (fresh h, t, ty in ((x == (h :: t) & xy == (h :: ty) & appendo t y ty))));

help xs ys zs ts = appendo xs ys ts & reverso zs ts;

? help ([Zero, Succ Zero]) ([Succ Zero, Zero, Succ (Succ Zero)]) ([Succ (Succ Zero), Zero, Succ Zero, Succ Zero, Zero, Succ Zero]) ts