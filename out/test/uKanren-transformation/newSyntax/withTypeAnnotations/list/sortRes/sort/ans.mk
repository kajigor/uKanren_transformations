filter (static dynamic) 
 sorto "lst" "reslst" = ((v."lst" = [] /\ v."reslst" = []) \/ fresh "h" "t" "l" "r" "g" "lres" "rres" ((v."lst" = (v."h" : v."t") /\ Unfold splito v."h" v."t" v."l" v."g" /\ Memo sorto v."l" v."lres" /\ Memo sorto v."r" v."rres" /\ Unfold appendo v."lres" ((v."h" : v."rres")) v."reslst")))
filter (static static dynamic dynamic) 
 splito "x" "xs" "l" "g" = ((v."xs" = [] /\ v."l" = [] /\ v."g" = []) \/ fresh "x1" "xs1" ((v."xs" = (v."x1" : v."xs1") /\ (fresh "l1" ((Unfold le v."x1" v."x" /\ v."l" = (v."x1" : v."l1") /\ Unfold splito v."x" v."xs1" v."l1" v."g")) \/ fresh "g1" ((Unfold gt v."x1" v."x" /\ v."g" = (v."x1" : v."g1") /\ Unfold splito v."x" v."xs1" v."l" v."g1"))))))
filter (dynamic dynamic dynamic) 
 appendo "x" "y" "xy" = ((v."x" = [] /\ v."y" = v."xy") \/ fresh "h" "t" "ty" ((v."x" = (v."h" : v."t") /\ v."xy" = (v."h" : v."ty") /\ Memo appendo v."t" v."y" v."ty")))
filter (static static) 
 gt "x" "y" = (fresh "z" ((v."x" = C Succ [v."z"] /\ v."y" = Zero)) \/ fresh "x'" "y'" ((v."x" = C Succ [v."x'"] /\ v."y" = C Succ [v."y'"] /\ Unfold gt v."x'" v."y'")))
filter (static static) 
 le "x" "y" = (v."x" = Zero \/ fresh "x'" "y'" ((v."x" = C Succ [v."x'"] /\ v."y" = C Succ [v."y'"] /\ Unfold le v."x'" v."y'")))
filter () 
 fail  = Memo fail 

fresh "lst" "reslst" (Memo sorto v."lst" v."reslst")