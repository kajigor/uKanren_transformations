filter (static dynamic) 
 reverso "x" "y" = ((v."x" = [] /\ v."y" = []) \/ fresh "h" "t" "rt" ((Memo reverso v."t" v."rt" /\ v."x" = (v."h" : v."t") /\ Unfold appendo v."rt" ((v."h" : [])) v."y")))
filter (static static dynamic) 
 appendo "x" "y" "xy" = ((v."x" = [] /\ v."y" = v."xy") \/ fresh "h" "t" "ty" ((v."x" = (v."h" : v."t") /\ v."xy" = (v."h" : v."ty") /\ Unfold appendo v."t" v."y" v."ty")))
filter () 
 fail  = Memo fail 

fresh "xs" "ys" "ts" ((Memo appendo v."xs" v."ys" v."ts" /\ Memo reverso v."ts" v."ys"))