:: rp t1 t2 =
  [u: {rotate t1 u} /\ {prune u t2}]

:: rotate t1 t2 = conde
  ([n: t1 === <leaf:n> /\ t2 === t1])
  ([l n r rl rr: t1 === <tree: l n r> /\ t2 === <tree: rl n rr> /\ {rotate l rl} /\ {rotate r rr}])
  ([l n r rl rr: t1 === <tree: l n r> /\ t2 === <tree: rr n rl> /\ {rotate l rl} /\ {rotate r rr}])

:: addo x y z =
  x === zero /\ z === y \/
  [x1 z1:
    x === succ x1 /\ z === succ z1 /\ {addo x1 y z1}]


:: prune t1 t2 = conde
  ([n: t1 === <leaf:n> /\ t2 === t1])
  ([l r z: z === zero /\ t1 === <tree: l z r> /\ t2 === <leaf: z>])
  ([l n r pl pr:
    n1 === succ n /\
    t1 === <tree: l n1 r> /\
    t2 === <tree: pr n1 pl> /\
    {rotate l pl} /\
    {rotate r pr}])

? {rp t1 t2}