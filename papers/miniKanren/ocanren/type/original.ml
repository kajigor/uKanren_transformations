open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 = let rec type term gamma ttype = (fresh (gamma1 t2type t1type t2 t1 t vt v y x) ((((term === bConst x) &&& (ttype === boolean ())) ||| (((term === iConst y) &&& (ttype === integer ())) ||| (((term === var v) &&& (elemo v gamma ttype)) ||| (((term === abs v vt t) &&& ((gamma1 === (Std.(%) (Pair.pair (v) (vt)) gamma)) &&& (type t gamma1 ttype))) ||| ((term === app t1 t2) &&& ((type t1 gamma (arrow t1type t2type)) &&& (type t2 gamma t2type))))))))) and elemo n s v = (fresh (n1 t h) ((((n === Std.Nat.zero) &&& ((s === (Std.(%) h t)) &&& (v === h))) ||| ((n === (Std.Nat.succ (n1))) &&& ((s === (Std.(%) h t)) &&& (elemo n1 t v)))))) in   (fresh (y x gamma t) ((type t gamma (arrow x y))))
