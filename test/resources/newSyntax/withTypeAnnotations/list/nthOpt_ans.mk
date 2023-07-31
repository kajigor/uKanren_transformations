filter (dynamic static dynamic) 
 nthOpt "xs" "n" "r" = fresh "h" "t" "x" (((v."xs" = [] /\ v."r" = None) \/ (v."xs" = (v."h" : v."t") /\ ((v."n" = Zero /\ v."r" = C Some [v."h"]) \/ (v."n" = C Succ [v."x"] /\ Unfold nthOpt v."t" v."x" v."r")))))
filter () 
 fail  = Memo fail 

fresh "xs" "n" "r" (Memo nthOpt v."xs" v."n" v."r")