filter (static static dynamic) 
 elemo n s v = fresh h t n' (((n = Zero /\ s = (h : t) /\ v = h) \/ (s = (h : t) /\ Delay (Unfold elemo n' t v) /\ n = C Succ [n'])))
filter (static static dynamic) 
 evalo st fm u = fresh x y v w z ((fm = C Lit [u] \/ (fm = C Var [z] /\ Delay (Unfold elemo z st u)) \/ (Unfold noto v u /\ Delay (Memo evalo st x v) /\ fm = C Neg [x]) \/ (Unfold oro v w u /\ Delay (Memo evalo st x v) /\ Delay (Memo evalo st y w) /\ fm = C Disj [x, y]) \/ (Unfold ando v w u /\ Delay (Memo evalo st x v) /\ Delay (Memo evalo st y w) /\ fm = C Conj [x, y]) \/ (Unfold implicationo v w u /\ Delay (Memo evalo st x v) /\ Delay (Memo evalo st y w) /\ fm = C Impl [x, y])))
filter (static static dynamic) 
 implicationo x y b = ((x = Falso /\ y = Trueo /\ b = Trueo) \/ (x = Falso /\ y = Falso /\ b = Trueo) \/ (x = Trueo /\ y = Trueo /\ b = Trueo) \/ (x = Trueo /\ y = Falso /\ b = Falso))
filter (static static) 
 noto x b = ((x = Trueo /\ b = Falso) \/ (x = Falso /\ b = Trueo))
filter (static static dynamic) 
 oro x y b = ((x = Trueo /\ y = Trueo /\ b = Trueo) \/ (x = Falso /\ y = Trueo /\ b = Trueo) \/ (x = Trueo /\ y = Falso /\ b = Trueo) \/ (x = Falso /\ y = Falso /\ b = Falso))
filter (static static dynamic) 
 ando x y b = ((x = Trueo /\ y = Trueo /\ b = Trueo) \/ (x = Falso /\ y = Trueo /\ b = Falso) \/ (x = Trueo /\ y = Falso /\ b = Falso) \/ (x = Falso /\ y = Falso /\ b = Falso))
filter () 
 fail  = Memo fail 

fresh x (Memo evalo [] (C Disj [x, x]) Trueo)