
nthOpt xs n r =
  (fresh h, t, x in
    xs == [] & r == None |
    xs == (h :: t) &
    (
      n == Zero & r == Some h |
      n == Succ x & nthOpt t x r
    )
  );

? nthOpt xs (Succ (Succ (Succ Zero))) r