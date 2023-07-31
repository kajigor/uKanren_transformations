filter (dynamic static dynamic) 
 gto "x" "y" "b" = ((v."x" = Zero /\ v."b" = Falso) \/ fresh "z" ((v."x" = C Succ [v."z"] /\ v."y" = Zero /\ v."b" = Trueo)) \/ fresh "x'" "y'" ((v."x" = C Succ [v."x'"] /\ v."y" = C Succ [v."y'"] /\ Unfold gto v."x'" v."y'" v."b")))
filter (dynamic static dynamic) 
 leo "x" "y" "b" = ((v."x" = Zero /\ v."b" = Trueo) \/ fresh "z" ((v."x" = C Succ [v."z"] /\ v."y" = Zero /\ v."b" = Falso)) \/ fresh "x'" "y'" ((v."x" = C Succ [v."x'"] /\ v."y" = C Succ [v."y'"] /\ Unfold leo v."x'" v."y'" v."b")))
filter () 
 fail  = Memo fail 

fresh "a" "b" ((Memo leo v."a" v."b" True /\ Memo gto v."b" v."a" True))