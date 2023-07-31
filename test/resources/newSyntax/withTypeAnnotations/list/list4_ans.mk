filter (static dynamic dynamic) 
 maxMino "x" "m" "l" = (Unfold maxo v."x" v."m" /\ Unfold mino v."x" v."l")
filter (static static dynamic) 
 mino1 "x" "n" "m" = ((v."x" = [] /\ v."m" = v."n") \/ fresh "h" "t" "z" ((v."x" = (v."h" : v."t") /\ Unfold leo v."h" v."n" Trueo /\ Unfold mino1 v."t" v."h" v."m")) \/ fresh "h" "t" "z" ((v."x" = (v."h" : v."t") /\ Unfold gto v."h" v."n" Trueo /\ Unfold mino1 v."t" v."n" v."m")))
filter (static dynamic) 
 mino "x" "m" = ((v."x" = [] /\ v."m" = Zero) \/ fresh "h" "t" ((v."x" = (v."h" : v."t") /\ Unfold mino1 v."t" v."h" v."m")))
filter (static static dynamic) 
 maxo1 "x" "n" "m" = ((v."x" = [] /\ v."m" = v."n") \/ fresh "h" "t" "z" ((v."x" = (v."h" : v."t") /\ Unfold leo v."h" v."n" Trueo /\ Unfold maxo1 v."t" v."n" v."m")) \/ fresh "h" "t" "z" ((v."x" = (v."h" : v."t") /\ Unfold gto v."h" v."n" Trueo /\ Unfold maxo1 v."t" v."h" v."m")))
filter (static dynamic) 
 maxo "x" "m" = Unfold maxo1 v."x" Zero v."m"
filter (static static dynamic) 
 gto "x" "y" "b" = ((v."x" = Zero /\ v."b" = Falso) \/ fresh "z" ((v."x" = C Succ [v."z"] /\ v."y" = Zero /\ v."b" = Trueo)) \/ fresh "x'" "y'" ((v."x" = C Succ [v."x'"] /\ v."y" = C Succ [v."y'"] /\ Unfold gto v."x'" v."y'" v."b")))
filter (static static dynamic) 
 leo "x" "y" "b" = ((v."x" = Zero /\ v."b" = Trueo) \/ fresh "z" ((v."x" = C Succ [v."z"] /\ v."y" = Zero /\ v."b" = Falso)) \/ fresh "x'" "y'" ((v."x" = C Succ [v."x'"] /\ v."y" = C Succ [v."y'"] /\ Unfold leo v."x'" v."y'" v."b")))
filter () 
 fail  = Memo fail 

fresh "x" "m" "l" (Memo maxMino v."x" v."m" v."l")
