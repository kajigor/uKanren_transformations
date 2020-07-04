open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 y3 = let rec check_uni z1 z2 z3 z4 = (fresh (fC fB fA) (((z1 === fA) &&& ((z2 === fB) &&& ((z3 === fC) &&& ((z4 === !!true) &&& (check_uni__1 fA fB fC))))))) and check_uni__1 z1 z2 z3 = ((fresh (fE fD fC fB fA) (((z1 === fA) &&& ((z2 === constr fB fC) &&& ((z3 === constr fD fE) &&& ((eq_nat__4 fB fD) &&& (forall2__2 fA fC fE))))))) ||| ((fresh (fE fD fC fB fA) (((z1 === (Std.(%) (fA) (fB))) &&& ((z2 === var_ fC) &&& ((z3 === constr fD fE) &&& (get_term_conj__9 fC fA fB fA fB fD fE)))))) ||| ((fresh (fE fD fC fB fA) (((z1 === (Std.(%) (fA) (fB))) &&& ((z2 === constr fC fD) &&& ((z3 === var_ fE) &&& (get_term_conj__6 fE fA fB fA fB fC fD)))))) ||| ((fresh (fE fD fC fB fA) (((z1 === (Std.(%) (fA) (fB))) &&& ((z2 === var_ fC) &&& ((z3 === var_ fD) &&& ((get_term__5 fC fA fB fE) &&& (check_uni__1 ((Std.(%) (fA) (fB))) fE (var_ fD)))))))) ||| (fresh (fC fB fA) (((z1 === fA) &&& ((z2 === var_ fB) &&& ((z3 === var_ fC) &&& ((get_term__3 fB fA) &&& ((get_term__3 fC fA) &&& (eq_nat__4 fB fC)))))))))))) and forall2__2 z1 z2 z3 = ((fresh (fA) (((z1 === fA) &&& ((z2 === (Std.List.nil ())) &&& (z3 === (Std.List.nil ())))))) ||| (fresh (fE fD fC fB fA) (((z1 === fA) &&& ((z2 === (Std.(%) (fB) (fC))) &&& ((z3 === (Std.(%) (fD) (fE))) &&& ((check_uni__1 fA fB fD) &&& (forall2__2 fA fC fE)))))))) and get_term__3 z1 z2 = ((fresh (fA) (((z1 === fA) &&& (z2 === (Std.List.nil ()))))) ||| ((fresh (fA) (((z1 === Nat.zero) &&& (z2 === (Std.(%) ((Option.none ())) (fA)))))) ||| (fresh (fC fB fA) (((z1 === (Std.Nat.succ (fA))) &&& ((z2 === (Std.(%) (fB) (fC))) &&& (get_term__3 fA fC))))))) and eq_nat__4 z1 z2 = (((z1 === Nat.zero) &&& (z2 === Nat.zero)) ||| (fresh (fB fA) (((z1 === (Std.Nat.succ (fA))) &&& ((z2 === (Std.Nat.succ (fB))) &&& (eq_nat__4 fA fB)))))) and get_term__5 z1 z2 z3 z4 = ((fresh (fB fA) (((z1 === Nat.zero) &&& ((z2 === (Option.some (fA))) &&& ((z3 === fB) &&& (z4 === fA)))))) ||| (fresh (fE fD fC fB fA) (((z1 === (Std.Nat.succ (fA))) &&& ((z2 === fB) &&& ((z3 === (Std.(%) (fC) (fD))) &&& ((z4 === fE) &&& (get_term__5 fA fC fD fE)))))))) and get_term_conj__6 z1 z2 z3 z4 z5 z6 z7 = ((fresh (fF fE fD fC fB fA) (((z1 === Nat.zero) &&& ((z2 === (Option.some (fA))) &&& ((z3 === fB) &&& ((z4 === fC) &&& ((z5 === fD) &&& ((z6 === fE) &&& ((z7 === fF) &&& (check_uni__7 fC fD fE fF fA)))))))))) ||| (fresh (fH fG fF fE fD fC fB fA) (((z1 === (Std.Nat.succ (fA))) &&& ((z2 === fB) &&& ((z3 === (Std.(%) (fC) (fD))) &&& ((z4 === fE) &&& ((z5 === fF) &&& ((z6 === fG) &&& ((z7 === fH) &&& (get_term_conj__6 fA fC fD fE fF fG fH))))))))))) and check_uni__7 z1 z2 z3 z4 z5 = ((fresh (fF fE fD fC fB fA) (((z1 === fA) &&& ((z2 === fB) &&& ((z3 === fC) &&& ((z4 === fD) &&& ((z5 === constr fE fF) &&& ((eq_nat__4 fC fE) &&& (forall2__8 fA fB fD fF))))))))) ||| (fresh (fE fD fC fB fA) (((z1 === fA) &&& ((z2 === fB) &&& ((z3 === fC) &&& ((z4 === fD) &&& ((z5 === var_ fE) &&& (get_term_conj__6 fE fA fB fA fB fC fD))))))))) and forall2__8 z1 z2 z3 z4 = ((fresh (fB fA) (((z1 === fA) &&& ((z2 === fB) &&& ((z3 === (Std.List.nil ())) &&& (z4 === (Std.List.nil ()))))))) ||| (fresh (fF fE fD fC fB fA) (((z1 === fA) &&& ((z2 === fB) &&& ((z3 === (Std.(%) (fC) (fD))) &&& ((z4 === (Std.(%) (fE) (fF))) &&& ((check_uni__1 ((Std.(%) (fA) (fB))) fC fE) &&& (forall2__8 fA fB fD fF))))))))) and get_term_conj__9 z1 z2 z3 z4 z5 z6 z7 = ((fresh (fF fE fD fC fB fA) (((z1 === Nat.zero) &&& ((z2 === (Option.some (fA))) &&& ((z3 === fB) &&& ((z4 === fC) &&& ((z5 === fD) &&& ((z6 === fE) &&& ((z7 === fF) &&& (check_uni__10 fC fD fA fE fF)))))))))) ||| (fresh (fH fG fF fE fD fC fB fA) (((z1 === (Std.Nat.succ (fA))) &&& ((z2 === fB) &&& ((z3 === (Std.(%) (fC) (fD))) &&& ((z4 === fE) &&& ((z5 === fF) &&& ((z6 === fG) &&& ((z7 === fH) &&& (get_term_conj__9 fA fC fD fE fF fG fH))))))))))) and check_uni__10 z1 z2 z3 z4 z5 = ((fresh (fF fE fD fC fB fA) (((z1 === fA) &&& ((z2 === fB) &&& ((z3 === constr fC fD) &&& ((z4 === fE) &&& ((z5 === fF) &&& ((eq_nat__4 fC fE) &&& (forall2__8 fA fB fD fF))))))))) ||| (fresh (fE fD fC fB fA) (((z1 === fA) &&& ((z2 === fB) &&& ((z3 === var_ fC) &&& ((z4 === fD) &&& ((z5 === fE) &&& (get_term_conj__9 fC fA fB fA fB fD fE))))))))) in            (check_uni y1 y2 y3 (!!true))
