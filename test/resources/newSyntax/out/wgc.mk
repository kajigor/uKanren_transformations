-- Source implementation: https://github.com/PLTools/OCanren/blob/89896740ff6fbe3256ef6706c78c74be5d8479f1/samples/WGC.ml
eval state moves state' =
  (moves == Nil) & (state == state') |
  (fresh move, moves', state'' in
    (moves == Cons move moves') &
    (step state move state'') &
    (eval state'' moves' state'))
;

swap state result =
  fresh l, r in
    state  == State l r &
    result == State r l;

step' left right move state' =
  fresh lm, lg, lw, lc, rm, rg, rw, rc in
    (left == Side lg lw lc lm) & (right == Side rg rw rc rm) &
    ( (move == Empty  )                  & (state' == State (Side lg lw lc Falso   ) (Side rg rw rc Trueo))    & safe state' |
      (move == Goat   ) & isGoat    left & (state' == State (Side Falso lw lc Falso) (Side Trueo rw rc Trueo)) & safe state' |
      (move == Wolf   ) & isWolf    left & (state' == State (Side lg Falso lc Falso) (Side rg Trueo rc Trueo)) & safe state' |
      (move == Cabbage) & isCabbage left & (state' == State (Side lg lw Falso Falso) (Side rg rw Trueo Trueo)) & safe state' )
      ;

step state move state' =
  fresh left, right in
    ( state == State left right ) &
    ( isMan left  & noMan right & step' left right move state' |
      isMan right & noMan left  & (fresh state'' in step' right left move state'' & swap state'' state'));

safe' side =
  isMan side |
  (noMan side &
    (noGoat side | (isGoat side & ((noCabbage side & noWolf side) |
                                   (isCabbage side & isWolf side)))));

safe state =
  fresh left, right in
    state == State left right &
    safe' left &
    safe' right;

isGoat side =
  fresh w, c, m in
    side == Side Trueo w c m;

noGoat side =
  fresh w, c, m in
    side == Side Falso w c m;

isWolf side =
  fresh g, w, c, m in
    side == Side g Trueo c m;

noWolf side =
  fresh g, c, m in
    side == Side g Falso c m;

isCabbage side =
  fresh g, w, m in
    side == Side g w Trueo m;

noCabbage side =
  fresh g, w, m in
    side == Side g w Falso m;

isMan side =
  fresh g, w, c, m in
    side == Side g w c Trueo;

noMan side =
  fresh g, w, c, m in
    side == Side g w c Falso;

moveGoat left right result =
  fresh lw, lc, rw, rc in
    left   == Side Trueo lw lc Trueo &
    right  == Side Falso rw rc Falso &
    result == State (Side Falso lw lc Falso) (Side Trueo rw rc Trueo);

moveWolf left right result =
  fresh lg, lc, rg, rc in
    left   == Side lg Trueo lc Trueo &
    right  == Side rg Falso rc Falso &
    result == State (Side lg Falso lc Falso) (Side rg Trueo rc Trueo);

moveCabbage left right result =
  fresh lg, lw, rg, rw in
    left   == Side lg lw Trueo Trueo &
    right  == Side rg rw Falso Falso &
    result == State (Side lg lw Falso Falso) (Side rg rw Trueo Trueo);

moveNothing left right result =
  fresh lg, lw, lc, rg, rw, rc in
    left   == Side lg lw lc Trueo &
    right  == Side rg rw rc Falso &
    result == State (Side lg lw lc Falso) (Side rg rw rc Trueo);

? initState  == State (Side Trueo Trueo Trueo Trueo) (Side Falso Falso Falso Falso) &
  finalState == State (Side Falso Falso Falso Falso) (Side Trueo Trueo Trueo Trueo) &
  -- moves == Cons goat1 (Cons empty1 (Cons wolf1 (Cons goat2 (Cons cabbage1 (Cons empty2 (Cons goat2 Nil)))))) &
  eval initState moves finalState
  -- eval initState moves (State (Side Trueo Trueo Trueo Falso) (Side Trueo Trueo Trueo Trueo))
