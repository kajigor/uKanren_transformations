fail() :- fail().
get(quad(Q, A, B, C), goat, Q).
get(quad(A, Q, B, C), wolf, Q).
get(quad(A, B, Q, C), cabbage, Q).
get(quad(A, B, C, Q), man, Q).
safe_0(Side) :- get(Side, man, true).
safe_0(Side) :- get(Side, goat, false), get(Side, man, false).
safe_0(Side) :- get(Side, cabbage, true), get(Side, wolf, true), get(Side, goat, true), get(Side, man, false).
safe_0(Side) :- get(Side, cabbage, false), get(Side, wolf, false), get(Side, goat, true), get(Side, man, false).
safe(pair(Left, Right)) :- safe_0(Left), safe_0(Right).
swap(pair(Left, Right), pair(Right, Left)).
step_0(quad(Lm, Lg, Lw, Lc), quad(Rm, Rg, Rw, Rc), pair(quad(Lm, Lg, Lw, false), quad(Rm, Rg, Rw, true)), empty) :- safe(pair(quad(Lm, Lg, Lw, false), quad(Rm, Rg, Rw, true))).
step_0(quad(Lm, Lg, Lw, Lc), quad(Rm, Rg, Rw, Rc), pair(quad(false, Lg, Lw, false), quad(true, Rg, Rw, true)), goat) :- get(quad(Lm, Lg, Lw, Lc), goat, true), safe(pair(quad(false, Lg, Lw, false), quad(true, Rg, Rw, true))).
step_0(quad(Lm, Lg, Lw, Lc), quad(Rm, Rg, Rw, Rc), pair(quad(Lm, false, Lw, false), quad(Rg, true, Rw, true)), wolf) :- get(quad(Lm, Lg, Lw, Lc), wolf, true), safe(pair(quad(Lm, false, Lw, false), quad(Rg, true, Rw, true))).
step_0(quad(Lm, Lg, Lw, Lc), quad(Rm, Rg, Rw, Rc), pair(quad(Lm, Lg, false, false), quad(Rm, Rg, true, true)), cabbage) :- get(quad(Lm, Lg, Lw, Lc), cabbage, true), safe(pair(quad(Lm, Lg, false, false), quad(Rm, Rg, true, true))).
step(pair(Left, Right), Move, State_0) :- get(Left, man, true), get(Right, man, false), step_0(Left, Right, State_0, Move).
step(pair(Left, Right), Move, State_0) :- get(Right, man, true), get(Left, man, false), step_0(Right, Left, State_0_0, Move), swap(State_0_0, State_0).
eval(State_0, nil, State_0).
eval(State, cons(Move, Moves_0), State_0) :- step(State, Move, State_0_0), eval(State_0_0, Moves_0, State_0).