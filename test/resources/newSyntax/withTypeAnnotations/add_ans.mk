filter () 
 fail  = Memo fail 
filter (dynamic static dynamic) 
 addo "x" "y" "z" = ((v."x" = Zero /\ v."z" = v."y") \/ fresh "x'" ((v."x" = C Succ [v."x'"] /\ Memo addo v."x'" (C Succ [v."y"]) v."z")))

fresh "x" "y" "z" (Memo addo v."x" v."y" v."z")