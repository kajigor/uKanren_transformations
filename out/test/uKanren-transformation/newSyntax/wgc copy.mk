-- Source implementation: https://github.com/PLTools/OCanren/blob/89896740ff6fbe3256ef6706c78c74be5d8479f1/samples/WGC.ml

nil state moves result =
  moves == Nil & state == result;

cons state moves result =
  fresh h, t, temp in
      moves == Cons h t &
      Delay step state h temp &
      Delay eval temp t result;

eval state moves result =
  nil state moves result |
  cons state moves result;

swap state result =
  fresh l, r in
    state  == State l r &
    result == State r l;

moveRight l r move result =
  (fresh lg, lw, lc, lm, rg, rw, rc, rm in
    l == Side lg lw lc lm &
    r == Side rg rw rc rm &
    ( (move == Empty  )               & (result == State (Side lg lw lc Falso   ) (Side rg rw rc Trueo))    |
      (move == Goat   ) & lg == Trueo & (result == State (Side Falso lw lc Falso) (Side Trueo rw rc Trueo)) |
      (move == Wolf   ) & lw == Trueo & (result == State (Side lg Falso lc Falso) (Side rg Trueo rc Trueo)) |
      (move == Cabbage) & lc == Trueo & (result == State (Side lg lw Falso Falso) (Side rg rw Trueo Trueo)) )  &
    safeState result
    );

moveLeft l r move result =
  fresh temp in
    moveRight r l move temp &
    swap temp result;

step state move result =
  fresh l, r in
    state == State l r &
    ( isMan l & noMan r & moveRight l r move result |
      noMan l & isMan r & moveLeft  l r move result
    );

safeState state =
  fresh l, r in
    state == State l r &
    safeSide l &
    safeSide r;

safeSide side =
  isMan side |
  noMan side & (
    (noGoat side) |
    (isGoat side & (noCabbage side & noWolf side | isCabbage side & isWolf side))
  );

-- (noGoat side ||| (isGoat side &&& ((noCabbage side &&& noWolf side) |||
--                                   (isCabbage side &&& isWolf side)) -- when all three present, nobody eats anybody?
--                 )
-- )

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
  step initState move1 result &
  step result move2 nextResult
  -- moves == Cons goat1 (Cons empty1 (Cons wolf1 (Cons goat2 (Cons cabbage1 (Cons empty2 (Cons goat2 Nil)))))) &
  -- eval initState moves result
  -- eval initState moves (State (Side Trueo Trueo Trueo Falso) (Side Trueo Trueo Trueo Trueo))
