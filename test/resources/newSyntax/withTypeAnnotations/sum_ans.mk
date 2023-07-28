filter (static dynamic) 
 evalo "fm" "r" = (v."fm" = C Num [v."r"] \/ fresh "x" "y" "xr" "yr" ((Memo evalo v."x" v."xr" /\ Memo evalo v."y" v."yr" /\ v."fm" = C Sum [v."x", v."y"] /\ Unfold addo v."xr" v."yr" v."r")))
filter (static dynamic dynamic) 
 addo "x" "y" "z" = ((v."x" = Zero /\ v."z" = v."y") \/ fresh "x'" ((v."x" = C Succ [v."x'"] /\ Unfold addo v."x'" (C Succ [v."y"]) v."z")))
filter () 
 fail  = Memo fail 

fresh "y" (Memo addo Zero v."y" (C Succ [C Succ [Zero]]))