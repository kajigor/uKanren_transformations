
filter (static static static)
get side x q =
  fresh a, b, c in
    side == Quad q a b c & x == Goat |
    side == Quad a q b c & x == Wolf |
    side == Quad a b q c & x == Cabbage |
    side == Quad a b c q & x == Man;

filter (static)
safe' side =
  get side Man True |
  (
    (
      get side Goat False |
      (
        (
          get side Cabbage True &
          get side Wolf True |
          get side Cabbage False &
          get side Wolf False
        ) &
        get side Goat True
      )
    ) &
    get side Man False
  );

filter (static)
safe state =
  fresh left, right in
    state == Pair left right &
    safe' left &
    safe' right;

filter (static dynamic)
swap state state' =
  fresh left, right in
    state == Pair left right & state' == Pair right left;

filter (static static dynamic dynamic)
step' left right state' move =
  fresh lm, lg, lw, lc, rm, rg, rw, rc in
    left == Quad lm lg lw lc &
    right == Quad rm rg rw rc &
    (
      move == Empty & state' == Pair (Quad lm lg lw False) (Quad rm rg rw True) & safe state' |
      move == Goat & get left Goat True & state' == Pair (Quad False lg lw False) (Quad True rg rw True) & safe state' |
      move == Wolf & get left Wolf True & state' == Pair (Quad lm False lw False) (Quad rg True rw True) & safe state' |
      move == Cabbage & get left Cabbage True & state' == Pair (Quad lm lg False False) (Quad rm rg True True) & safe state'
    );

filter (static dynamic dynamic)
step state move state' =
  fresh left, right in
    state == Pair left right &
    (
      get left Man True &
      get right Man False &
      step' left right state' move |
      (
        fresh state'' in
          get right Man True &
          get left Man False &
          step' right left state'' move &
          swap state'' state'
      )
    );

filter (static dynamic static)
eval state moves state' =
  moves == Nil & state == state' |
  (
    fresh move, moves', state'' in
      step state move state'' &
      eval state'' moves' state' &
      moves == (move :: moves')
  );

? eval (Pair (Quad True True True True) (Quad False False False False)) x (Pair (Quad False False False False) (Quad True True True True))
