open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = let rec type_ term gamma ttype = (fresh (els thn cond btype1 btype body bound t1 t r l n m v y x) ((((term === bConst_ x) &&& (ttype === (Option.some (boolean ())))) ||| (((term === iConst_ y) &&& (ttype === (Option.some (integer ())))) ||| (((term === var_ v) &&& (idx v gamma ttype)) ||| (((term === plus_ m n) &&& ((type_ m gamma ((Option.some (integer ())))) &&& ((type_ n gamma ((Option.some (integer ())))) &&& (ttype === (Option.some (integer ())))))) ||| (((term === mult_ m n) &&& ((type_ m gamma ((Option.some (integer ())))) &&& ((type_ n gamma ((Option.some (integer ())))) &&& (ttype === (Option.some (integer ())))))) ||| (((term === equal_ l r) &&& ((type_ l gamma t) &&& ((type_ r gamma t) &&& ((ttype === (Option.some (boolean ()))) &&& (t === (Option.some (t1))))))) ||| (((term === less_ l r) &&& ((type_ l gamma ((Option.some (integer ())))) &&& ((type_ r gamma ((Option.some (integer ())))) &&& (ttype === (Option.some (boolean ())))))) ||| (((term === let_ bound body) &&& ((type_ bound gamma btype) &&& ((btype === (Option.some (btype1))) &&& (type_ body ((Std.(%) (btype1) (gamma))) ttype)))) ||| ((term === if_ cond thn els) &&& ((type_ cond gamma ((Option.some (boolean ())))) &&& ((type_ thn gamma ttype) &&& (type_ els gamma ttype)))))))))))))) and idx k xs v = (fresh (k1 t h) ((((k === Std.Nat.zero) &&& ((xs === (Std.List.nil ())) &&& (v === (Option.none ())))) ||| (((k === Std.Nat.zero) &&& ((xs === (Std.(%) (h) (t))) &&& (v === (Option.some (h))))) ||| ((k === (Std.Nat.succ (k1))) &&& ((xs === (Std.(%) (h) (t))) &&& (idx k1 t v))))))) in   (((type_ x0 ((Std.List.nil ())) ((Option.some (integer ()))))))
