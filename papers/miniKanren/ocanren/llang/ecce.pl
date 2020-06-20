
/* Specialised program generated by ECCE 2.0 */
/* PD Goal: A */
/* Parameters: Abs:l InstCheck:v Msv:s NgSlv:g Part:e Prun:n Sel:t Whstl:f Raf:yesFar:yes Dce:yes Poly:y Dpu:yes ParAbs:yes Msvp:no Rrc:yes */
/* Transformation time: 15820 ms */
/* Unfolding time: 11420 ms */
/* Post-Processing time: 15820 ms */

/* Specialised Predicates:
type__1(A,B) :- type(A,B,integer).
elemo__2(A,B,C,D) :- elemo(A,cons(B,C),D).
type_conj__3(A,B,C,D,E) :- type(A,B,boolean), type(C,B,D), type(E,B,D).
type_conj__4(A,B,C,D,E,F) :- type(A,B,G1), type(C,cons(pair(D,G1),B),boolean), type(E,B,F).
type_conj__5(A,B,C,D,E,F) :- type(A,B,G1), type(C,cons(pair(D,G1),B),H1), type(E,cons(pair(F,H1),B),boolean).
type__6(A,B,C,D) :- type(A,cons(pair(B,C),D),boolean).
type_conj__7(A,B,C,D,E,F,G,H) :- type(A,cons(pair(B,C),D),E), type(F,cons(pair(G,H),cons(pair(B,C),D)),boolean).
elemo_conj__8(A,B,C,D,E,F,G,H) :- elemo(A,cons(pair(B,C),D),E), type(F,cons(pair(G,H),cons(pair(B,C),D)),boolean).
type_conj__9(A,B,C,D,E,F,G) :- type(A,B,H1), type(C,cons(pair(D,H1),B),I1), type(E,cons(pair(F,I1),B),G).
type_conj__10(A,B,C,D,E,F,G,H) :- type(A,cons(pair(B,C),D),I1), type(E,cons(pair(F,I1),G),H).
elemo_conj__11(A,B,C,D,E,F,G) :- elemo(A,cons(B,C),H1), type(D,cons(pair(E,H1),F),G).
elemo_conj__12(A,B,C,D,E,F,G,H,I,J) :- elemo(A,cons(B,C),K1), type(D,cons(pair(E,K1),cons(F,G)),L1), type(H,cons(pair(I,L1),cons(F,G)),J).
type_conj__13(A,B,C,D,E,F,G,H,I,J) :- type(A,cons(pair(B,C),cons(D,E)),K1), type(F,cons(pair(G,K1),cons(H,I)),J).
elemo_conj__14(A,B,C,D,E,F,G,H,I) :- elemo(A,cons(B,C),J1), type(D,cons(pair(E,J1),cons(F,G)),boolean), type(H,cons(F,G),I).
type__15(A,B,C,D,E) :- type(A,cons(pair(B,C),cons(D,E)),boolean).
type__16(A,B,C) :- type(A,B,C).
type_conj__17(A,B,C,D,E,F,G,H,I) :- type(A,cons(pair(B,C),cons(D,E)),F), type(G,cons(pair(H,I),cons(pair(B,C),cons(D,E))),boolean).
elemo_conj__18(A,B,C,D,E,F,G,H,I) :- elemo(A,cons(pair(B,C),cons(D,E)),F), type(G,cons(pair(H,I),cons(pair(B,C),cons(D,E))),boolean).
type_conj__19(A,B,C,D,E,F) :- type(A,B,C), type(D,cons(pair(E,F),B),integer).
*/

type(A,B,integer) :-
  type__1(A,B).
type__1(iConst(A),B).
type__1(var(A),cons(B,C)) :-
  elemo__2(A,B,C,integer).
type__1(plus(A,B),C) :-
  type__1(A,C),
  type__1(B,C).
type__1(mult(A,B),C) :-
  type__1(A,C),
  type__1(B,C).
type__1(let(A,B,C),D) :-
  type_conj__19(B,D,E,C,A,E).
type__1(if(A,B,C),D) :-
  type_conj__3(A,D,B,integer,C).
elemo__2(o,A,B,A).
elemo__2(s(A),B,cons(C,D),E) :-
  elemo__2(A,C,D,E).
type_conj__3(bConst(A),B,C,D,E) :-
  type__16(C,B,D),
  type__16(E,B,D).
type_conj__3(var(A),cons(B,C),D,E,F) :-
  elemo__2(A,B,C,boolean),
  type__16(D,cons(B,C),E),
  type__16(F,cons(B,C),E).
type_conj__3(eq(A,B),C,D,E,F) :-
  type__16(A,C,G),
  type__16(B,C,G),
  type__16(D,C,E),
  type__16(F,C,E).
type_conj__3(lt(A,B),C,D,E,F) :-
  type__16(A,C,G),
  type__16(B,C,G),
  type__16(D,C,E),
  type__16(F,C,E).
type_conj__3(let(A,B,C),D,E,F,G) :-
  type_conj__4(B,D,C,A,E,F),
  type__16(G,D,F).
type_conj__3(if(A,B,C),D,E,F,G) :-
  type_conj__3(A,D,B,boolean,C),
  type__16(E,D,F),
  type__16(G,D,F).
type_conj__4(bConst(A),B,C,D,E,F) :-
  type__6(C,D,boolean,B),
  type__16(E,B,F).
type_conj__4(iConst(A),B,C,D,E,F) :-
  type__6(C,D,integer,B),
  type__16(E,B,F).
type_conj__4(var(A),cons(B,C),D,E,F,G) :-
  elemo_conj__14(A,B,C,D,E,B,C,F,G).
type_conj__4(plus(A,B),C,D,E,F,G) :-
  type__1(A,C),
  type__1(B,C),
  type__6(D,E,integer,C),
  type__16(F,C,G).
type_conj__4(mult(A,B),C,D,E,F,G) :-
  type__1(A,C),
  type__1(B,C),
  type__6(D,E,integer,C),
  type__16(F,C,G).
type_conj__4(eq(A,B),C,D,E,F,G) :-
  type__16(A,C,H),
  type__16(B,C,H),
  type__6(D,E,boolean,C),
  type__16(F,C,G).
type_conj__4(lt(A,B),C,D,E,F,G) :-
  type__16(A,C,H),
  type__16(B,C,H),
  type__6(D,E,boolean,C),
  type__16(F,C,G).
type_conj__4(let(A,B,C),D,E,F,G,H) :-
  type_conj__5(B,D,C,A,E,F),
  type__16(G,D,H).
type_conj__4(if(A,B,C),D,E,F,G,H) :-
  type_conj__3(A,D,B,I,C),
  type__6(E,F,I,D),
  type__16(G,D,H).
type_conj__5(bConst(A),B,C,D,E,F) :-
  type__16(C,cons(pair(D,boolean),B),G),
  type__16(E,cons(pair(F,G),B),boolean).
type_conj__5(iConst(A),B,C,D,E,F) :-
  type__16(C,cons(pair(D,integer),B),G),
  type__16(E,cons(pair(F,G),B),boolean).
type_conj__5(var(A),cons(B,C),D,E,F,G) :-
  elemo_conj__12(A,B,C,D,E,B,C,F,G,boolean).
type_conj__5(plus(A,B),C,D,E,F,G) :-
  type__1(A,C),
  type__1(B,C),
  type__16(D,cons(pair(E,integer),C),H),
  type__6(F,G,H,C).
type_conj__5(mult(A,B),C,D,E,F,G) :-
  type__1(A,C),
  type__1(B,C),
  type__16(D,cons(pair(E,integer),C),H),
  type__6(F,G,H,C).
type_conj__5(eq(A,B),C,D,E,F,G) :-
  type__16(A,C,H),
  type__16(B,C,H),
  type__16(D,cons(pair(E,boolean),C),I),
  type__6(F,G,I,C).
type_conj__5(lt(A,B),C,D,E,F,G) :-
  type__16(A,C,H),
  type__16(B,C,H),
  type__16(D,cons(pair(E,boolean),C),I),
  type__6(F,G,I,C).
type_conj__5(let(A,B,C),D,E,F,G,H) :-
  type_conj__9(B,D,C,A,E,F,I),
  type__6(G,H,I,D).
type_conj__5(if(A,B,C),D,E,F,G,H) :-
  type_conj__3(A,D,B,I,C),
  type_conj__10(E,F,I,D,G,H,D,boolean).
type__6(bConst(A),B,C,D).
type__6(var(s(A)),B,C,cons(D,E)) :-
  elemo__2(A,D,E,boolean).
type__6(eq(A,B),C,D,E) :-
  type__16(A,cons(pair(C,D),E),F),
  type__16(B,cons(pair(C,D),E),F).
type__6(lt(A,B),C,D,E) :-
  type__16(A,cons(pair(C,D),E),F),
  type__16(B,cons(pair(C,D),E),F).
type__6(let(A,B,C),D,E,F) :-
  type_conj__7(B,D,E,F,G,C,A,G).
type__6(if(A,B,C),D,E,F) :-
  type__6(A,D,E,F),
  type__6(B,D,E,F),
  type__6(C,D,E,F).
type_conj__7(bConst(A),B,C,D,boolean,E,F,G) :-
  type__6(E,F,G,cons(pair(B,C),D)).
type_conj__7(iConst(A),B,C,D,integer,E,F,G) :-
  type__6(E,F,G,cons(pair(B,C),D)).
type_conj__7(var(A),B,C,D,E,F,G,H) :-
  elemo_conj__8(A,B,C,D,E,F,G,H).
type_conj__7(plus(A,B),C,D,E,integer,F,G,H) :-
  type__1(A,cons(pair(C,D),E)),
  type__1(B,cons(pair(C,D),E)),
  type__6(F,G,H,cons(pair(C,D),E)).
type_conj__7(mult(A,B),C,D,E,integer,F,G,H) :-
  type__1(A,cons(pair(C,D),E)),
  type__1(B,cons(pair(C,D),E)),
  type__6(F,G,H,cons(pair(C,D),E)).
type_conj__7(eq(A,B),C,D,E,boolean,F,G,H) :-
  type__16(A,cons(pair(C,D),E),I),
  type_conj__7(B,C,D,E,I,F,G,H).
type_conj__7(lt(A,B),C,D,E,boolean,F,G,H) :-
  type__16(A,cons(pair(C,D),E),I),
  type_conj__7(B,C,D,E,I,F,G,H).
type_conj__7(let(A,B,C),D,E,F,G,H,I,J) :-
  type__16(B,cons(pair(D,E),F),K),
  type__16(C,cons(pair(A,K),cons(pair(D,E),F)),G),
  type__6(H,I,J,cons(pair(D,E),F)).
type_conj__7(if(A,B,C),D,E,F,G,H,I,J) :-
  type__6(A,D,E,F),
  type__16(B,cons(pair(D,E),F),G),
  type__16(C,cons(pair(D,E),F),G),
  type__6(H,I,J,cons(pair(D,E),F)).
elemo_conj__8(o,A,B,C,pair(A,B),D,E,F) :-
  type__6(D,E,F,cons(pair(A,B),C)).
elemo_conj__8(s(A),B,C,cons(D,E),F,G,H,I) :-
  elemo__2(A,D,E,F),
  type__6(G,H,I,cons(pair(B,C),cons(D,E))).
type_conj__9(bConst(A),B,C,D,E,F,G) :-
  type__16(C,cons(pair(D,boolean),B),H),
  type__16(E,cons(pair(F,H),B),G).
type_conj__9(iConst(A),B,C,D,E,F,G) :-
  type__16(C,cons(pair(D,integer),B),H),
  type__16(E,cons(pair(F,H),B),G).
type_conj__9(var(A),cons(B,C),D,E,F,G,H) :-
  elemo_conj__12(A,B,C,D,E,B,C,F,G,H).
type_conj__9(plus(A,B),C,D,E,F,G,H) :-
  type__1(A,C),
  type__1(B,C),
  type__16(D,cons(pair(E,integer),C),I),
  type__16(F,cons(pair(G,I),C),H).
type_conj__9(mult(A,B),C,D,E,F,G,H) :-
  type__1(A,C),
  type__1(B,C),
  type__16(D,cons(pair(E,integer),C),I),
  type__16(F,cons(pair(G,I),C),H).
type_conj__9(eq(A,B),C,D,E,F,G,H) :-
  type__16(A,C,I),
  type__16(B,C,I),
  type__16(D,cons(pair(E,boolean),C),J),
  type__16(F,cons(pair(G,J),C),H).
type_conj__9(lt(A,B),C,D,E,F,G,H) :-
  type__16(A,C,I),
  type__16(B,C,I),
  type__16(D,cons(pair(E,boolean),C),J),
  type__16(F,cons(pair(G,J),C),H).
type_conj__9(let(A,B,C),D,E,F,G,H,I) :-
  type_conj__9(B,D,C,A,E,F,J),
  type__16(G,cons(pair(H,J),D),I).
type_conj__9(if(A,B,C),D,E,F,G,H,I) :-
  type_conj__3(A,D,B,J,C),
  type_conj__10(E,F,J,D,G,H,D,I).
type_conj__10(bConst(A),B,C,D,E,F,G,H) :-
  type__16(E,cons(pair(F,boolean),G),H).
type_conj__10(iConst(A),B,C,D,E,F,G,H) :-
  type__16(E,cons(pair(F,integer),G),H).
type_conj__10(var(A),B,C,D,E,F,G,H) :-
  elemo_conj__11(A,pair(B,C),D,E,F,G,H).
type_conj__10(plus(A,B),C,D,E,F,G,H,I) :-
  type__1(A,cons(pair(C,D),E)),
  type__1(B,cons(pair(C,D),E)),
  type__16(F,cons(pair(G,integer),H),I).
type_conj__10(mult(A,B),C,D,E,F,G,H,I) :-
  type__1(A,cons(pair(C,D),E)),
  type__1(B,cons(pair(C,D),E)),
  type__16(F,cons(pair(G,integer),H),I).
type_conj__10(eq(A,B),C,D,E,F,G,H,I) :-
  type__16(A,cons(pair(C,D),E),J),
  type__16(B,cons(pair(C,D),E),J),
  type__16(F,cons(pair(G,boolean),H),I).
type_conj__10(lt(A,B),C,D,E,F,G,H,I) :-
  type__16(A,cons(pair(C,D),E),J),
  type__16(B,cons(pair(C,D),E),J),
  type__16(F,cons(pair(G,boolean),H),I).
type_conj__10(let(A,B,C),D,E,F,G,H,I,J) :-
  type_conj__10(B,D,E,F,C,A,cons(pair(D,E),F),K),
  type__16(G,cons(pair(H,K),I),J).
type_conj__10(if(A,B,C),D,E,F,G,H,I,J) :-
  type__6(A,D,E,F),
  type__16(B,cons(pair(D,E),F),K),
  type__16(C,cons(pair(D,E),F),K),
  type__16(G,cons(pair(H,K),I),J).
elemo_conj__11(o,A,B,C,D,E,F) :-
  type__16(C,cons(pair(D,A),E),F).
elemo_conj__11(s(A),B,cons(C,D),E,F,G,H) :-
  elemo_conj__11(A,C,D,E,F,G,H).
elemo_conj__12(o,A,B,C,D,E,F,G,H,I) :-
  type_conj__13(C,D,A,E,F,G,H,E,F,I).
elemo_conj__12(s(A),B,cons(C,D),E,F,G,H,I,J,K) :-
  elemo_conj__12(A,C,D,E,F,G,H,I,J,K).
type_conj__13(bConst(A),B,C,D,E,F,G,H,I,J) :-
  type__16(F,cons(pair(G,boolean),cons(H,I)),J).
type_conj__13(iConst(A),B,C,D,E,F,G,H,I,J) :-
  type__16(F,cons(pair(G,integer),cons(H,I)),J).
type_conj__13(var(A),B,C,D,E,F,G,H,I,J) :-
  elemo_conj__11(A,pair(B,C),cons(D,E),F,G,cons(H,I),J).
type_conj__13(plus(A,B),C,D,E,F,G,H,I,J,K) :-
  type__1(A,cons(pair(C,D),cons(E,F))),
  type__1(B,cons(pair(C,D),cons(E,F))),
  type__16(G,cons(pair(H,integer),cons(I,J)),K).
type_conj__13(mult(A,B),C,D,E,F,G,H,I,J,K) :-
  type__1(A,cons(pair(C,D),cons(E,F))),
  type__1(B,cons(pair(C,D),cons(E,F))),
  type__16(G,cons(pair(H,integer),cons(I,J)),K).
type_conj__13(eq(A,B),C,D,E,F,G,H,I,J,K) :-
  type__16(A,cons(pair(C,D),cons(E,F)),L),
  type__16(B,cons(pair(C,D),cons(E,F)),L),
  type__16(G,cons(pair(H,boolean),cons(I,J)),K).
type_conj__13(lt(A,B),C,D,E,F,G,H,I,J,K) :-
  type__16(A,cons(pair(C,D),cons(E,F)),L),
  type__16(B,cons(pair(C,D),cons(E,F)),L),
  type__16(G,cons(pair(H,boolean),cons(I,J)),K).
type_conj__13(let(A,B,C),D,E,F,G,H,I,J,K,L) :-
  type_conj__13(B,D,E,F,G,C,A,pair(D,E),cons(F,G),M),
  type__16(H,cons(pair(I,M),cons(J,K)),L).
type_conj__13(if(A,B,C),D,E,F,G,H,I,J,K,L) :-
  type_conj__3(A,cons(pair(D,E),cons(F,G)),B,M,C),
  type__16(H,cons(pair(I,M),cons(J,K)),L).
elemo_conj__14(o,A,B,C,D,E,F,G,H) :-
  type__15(C,D,A,E,F),
  type__16(G,cons(E,F),H).
elemo_conj__14(s(A),B,cons(C,D),E,F,G,H,I,J) :-
  elemo_conj__14(A,C,D,E,F,G,H,I,J).
type__15(bConst(A),B,C,D,E).
type__15(var(s(A)),B,C,D,E) :-
  elemo__2(A,D,E,boolean).
type__15(eq(A,B),C,D,E,F) :-
  type__16(A,cons(pair(C,D),cons(E,F)),G),
  type__16(B,cons(pair(C,D),cons(E,F)),G).
type__15(lt(A,B),C,D,E,F) :-
  type__16(A,cons(pair(C,D),cons(E,F)),G),
  type__16(B,cons(pair(C,D),cons(E,F)),G).
type__15(let(A,B,C),D,E,F,G) :-
  type_conj__17(B,D,E,F,G,H,C,A,H).
type__15(if(A,B,C),D,E,F,G) :-
  type__15(A,D,E,F,G),
  type__15(B,D,E,F,G),
  type__15(C,D,E,F,G).
type__16(bConst(A),B,boolean).
type__16(iConst(A),B,integer).
type__16(var(A),cons(B,C),D) :-
  elemo__2(A,B,C,D).
type__16(plus(A,B),C,integer) :-
  type__1(A,C),
  type__1(B,C).
type__16(mult(A,B),C,integer) :-
  type__1(A,C),
  type__1(B,C).
type__16(eq(A,B),C,boolean) :-
  type__16(A,C,D),
  type__16(B,C,D).
type__16(lt(A,B),C,boolean) :-
  type__16(A,C,D),
  type__16(B,C,D).
type__16(let(A,B,C),D,E) :-
  type__16(B,D,F),
  type__16(C,cons(pair(A,F),D),E).
type__16(if(A,B,C),D,E) :-
  type_conj__3(A,D,B,E,C).
type_conj__17(bConst(A),B,C,D,E,boolean,F,G,H) :-
  type__6(F,G,H,cons(pair(B,C),cons(D,E))).
type_conj__17(iConst(A),B,C,D,E,integer,F,G,H) :-
  type__6(F,G,H,cons(pair(B,C),cons(D,E))).
type_conj__17(var(A),B,C,D,E,F,G,H,I) :-
  elemo_conj__18(A,B,C,D,E,F,G,H,I).
type_conj__17(plus(A,B),C,D,E,F,integer,G,H,I) :-
  type__1(A,cons(pair(C,D),cons(E,F))),
  type__1(B,cons(pair(C,D),cons(E,F))),
  type__6(G,H,I,cons(pair(C,D),cons(E,F))).
type_conj__17(mult(A,B),C,D,E,F,integer,G,H,I) :-
  type__1(A,cons(pair(C,D),cons(E,F))),
  type__1(B,cons(pair(C,D),cons(E,F))),
  type__6(G,H,I,cons(pair(C,D),cons(E,F))).
type_conj__17(eq(A,B),C,D,E,F,boolean,G,H,I) :-
  type__16(A,cons(pair(C,D),cons(E,F)),J),
  type_conj__17(B,C,D,E,F,J,G,H,I).
type_conj__17(lt(A,B),C,D,E,F,boolean,G,H,I) :-
  type__16(A,cons(pair(C,D),cons(E,F)),J),
  type_conj__17(B,C,D,E,F,J,G,H,I).
type_conj__17(let(A,B,C),D,E,F,G,H,I,J,K) :-
  type__16(B,cons(pair(D,E),cons(F,G)),L),
  type__16(C,cons(pair(A,L),cons(pair(D,E),cons(F,G))),H),
  type__6(I,J,K,cons(pair(D,E),cons(F,G))).
type_conj__17(if(A,B,C),D,E,F,G,H,I,J,K) :-
  type_conj__3(A,cons(pair(D,E),cons(F,G)),B,H,C),
  type__6(I,J,K,cons(pair(D,E),cons(F,G))).
elemo_conj__18(o,A,B,C,D,pair(A,B),E,F,G) :-
  type__6(E,F,G,cons(pair(A,B),cons(C,D))).
elemo_conj__18(s(A),B,C,D,E,F,G,H,I) :-
  elemo__2(A,D,E,F),
  type__6(G,H,I,cons(pair(B,C),cons(D,E))).
type_conj__19(bConst(A),B,boolean,C,D,E) :-
  type__1(C,cons(pair(D,E),B)).
type_conj__19(iConst(A),B,integer,C,D,E) :-
  type__1(C,cons(pair(D,E),B)).
type_conj__19(var(A),cons(B,C),D,E,F,G) :-
  elemo__2(A,B,C,D),
  type__1(E,cons(pair(F,G),cons(B,C))).
type_conj__19(plus(A,B),C,integer,D,E,F) :-
  type__1(A,C),
  type__1(B,C),
  type__1(D,cons(pair(E,F),C)).
type_conj__19(mult(A,B),C,integer,D,E,F) :-
  type__1(A,C),
  type__1(B,C),
  type__1(D,cons(pair(E,F),C)).
type_conj__19(eq(A,B),C,boolean,D,E,F) :-
  type__16(A,C,G),
  type_conj__19(B,C,G,D,E,F).
type_conj__19(lt(A,B),C,boolean,D,E,F) :-
  type__16(A,C,G),
  type_conj__19(B,C,G,D,E,F).
type_conj__19(let(A,B,C),D,E,F,G,H) :-
  type__16(B,D,I),
  type__16(C,cons(pair(A,I),D),E),
  type__1(F,cons(pair(G,H),D)).
type_conj__19(if(A,B,C),D,E,F,G,H) :-
  type_conj__3(A,D,B,E,C),
  type__1(F,cons(pair(G,H),D)).