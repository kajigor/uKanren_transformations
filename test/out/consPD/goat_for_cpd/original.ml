open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec fail () = fail 
  and get side x q = (fresh (a b c) ((((side === (quad q a b c)) &&& (x === (goat ()))) ||| ((side === (quad a q b c)) &&& (x === (wolf ()))) ||| ((side === (quad a b q c)) &&& (x === (cabbage ()))) ||| ((side === (quad a b c q)) &&& (x === (man ())))))) 
  and safe' side = ((get side ((man ())) !!true) ||| (((get side ((goat ())) !!false) ||| ((((get side ((cabbage ())) !!true) &&& (get side ((wolf ())) !!true)) ||| ((get side ((cabbage ())) !!false) &&& (get side ((wolf ())) !!false))) &&& (get side ((goat ())) !!true))) &&& (get side ((man ())) !!false))) 
  and safe state = (fresh (left right) (((state === (pair left right)) &&& (safe' left) &&& (safe' right)))) 
  and swap state state' = (fresh (left right) (((state === (pair left right)) &&& (state' === (pair right left))))) 
  and step' left right state' move = (fresh (lm lg lw lc rm rg rw rc) (((left === (quad lm lg lw lc)) &&& (right === (quad rm rg rw rc)) &&& (((move === (empty ())) &&& (state' === (pair ((quad lm lg lw !!false)) ((quad rm rg rw !!true)))) &&& (safe state')) ||| ((move === (goat ())) &&& (get left ((goat ())) !!true) &&& (state' === (pair ((quad !!false lg lw !!false)) ((quad !!true rg rw !!true)))) &&& (safe state')) ||| ((move === (wolf ())) &&& (get left ((wolf ())) !!true) &&& (state' === (pair ((quad lm !!false lw !!false)) ((quad rg !!true rw !!true)))) &&& (safe state')) ||| ((move === (cabbage ())) &&& (get left ((cabbage ())) !!true) &&& (state' === (pair ((quad lm lg !!false !!false)) ((quad rm rg !!true !!true)))) &&& (safe state')))))) 
  and step state move state' = (fresh (left right) (((state === (pair left right)) &&& (((get left ((man ())) !!true) &&& (get right ((man ())) !!false) &&& (step' left right state' move)) ||| (fresh (state'') (((get right ((man ())) !!true) &&& (get left ((man ())) !!false) &&& (step' right left state'' move) &&& (swap state'' state')))))))) 
  and eval state moves state' = (((moves === (List.nil ())) &&& (state === state')) ||| (fresh (move moves' state'') (((step state move state'') &&& (eval state'' moves' state') &&& (moves === (move % moves')))))) 
  in         (fresh (x) ((eval ((pair ((quad !!true !!true !!true !!true)) ((quad !!false !!false !!false !!false)))) x ((pair ((quad !!false !!false !!false !!false)) ((quad !!true !!true !!true !!true)))))))
