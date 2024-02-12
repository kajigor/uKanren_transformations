open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec fail () = fail 
  and solve rules res = ((res === (List.nil ())) ||| (fresh (ngh ngt clause ngbody clause) (((res === (ngh % ngt)) &&& (non_ground_member ((term ((clause ())) ((ngh % ngbody)))) rules) &&& (solve rules ngbody) &&& (solve rules ngt))))) 
  and non_ground_member ngx ngl = (fresh (grh grt) (((ngl === (grh % grt)) &&& ((non_ground_member ngx grt) ||| (make_non_ground grh ngx))))) 
  and make_non_ground g ng = (fresh (sub) ((mkng g ng ((List.nil ())) sub))) 
  and neq x y = ((fresh (t) (((x === (zero ())) &&& (y === (succ t))))) ||| (fresh (t) (((x === (succ t)) &&& (y === (zero ()))))) ||| (fresh (t1 t2) (((x === (succ t1)) &&& (y === (succ t2)) &&& (neq t1 t2))))) 
  and mkng term1 term2 inSub outSub = ((fresh (n) (((term1 === (var n)) &&& (inSub === (List.nil ())) &&& (outSub === (((sub n term2)) % ((List.nil ()))))))) ||| (fresh (n t) (((term1 === (var n)) &&& (inSub === (((sub n term2)) % t)) &&& (outSub === (((sub n term2)) % t))))) ||| (fresh (n m t t1 y) (((term1 === (var n)) &&& (inSub === (((sub m y)) % t)) &&& (outSub === (((sub m y)) % t1)) &&& (neq n m) &&& (mkng term1 term2 t t1)))) ||| (fresh (f args iArgs) (((term1 === (term f args)) &&& (term2 === (term f iArgs)) &&& (l_mkng args iArgs inSub outSub))))) 
  and l_mkng lst1 lst2 inSub outSub = (((lst1 === (List.nil ())) &&& (lst2 === (List.nil ())) &&& (inSub === outSub)) ||| (fresh (h t ih it intSub) (((lst1 === (h % t)) &&& (lst2 === (ih % it)) &&& (mkng h ih inSub intSub) &&& (l_mkng t it intSub outSub))))) 
  in        (fresh (x y z) ((solve ((((term ((clause ())) ((((term ((app ())) ((((term ((null ())) ((List.nil ())))) % ((((var Nat.zero)) % ((((var Nat.zero)) % ((List.nil ())))))))))) % ((List.nil ())))))) % ((((term ((clause ())) ((((term ((app ())) ((((term ((cons ())) ((((var Nat.zero)) % ((((var ((Nat.succ Nat.zero)))) % ((List.nil ())))))))) % ((((var ((Nat.succ ((Nat.succ Nat.zero)))))) % ((((term ((cons ())) ((((var Nat.zero)) % ((((var ((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))))) % ((List.nil ())))))))) % ((List.nil ())))))))))) % ((((term ((app ())) ((((var ((Nat.succ Nat.zero)))) % ((((var ((Nat.succ ((Nat.succ Nat.zero)))))) % ((((var ((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))))) % ((List.nil ())))))))))) % ((List.nil ())))))))) % ((List.nil ())))))) ((((term ((app ())) ((x % ((y % ((z % ((List.nil ())))))))))) % ((List.nil ())))))))
