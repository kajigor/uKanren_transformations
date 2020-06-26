open GT
open OCanren

@type 'a gnat =
| O
| S of 'a
with show, gmap

@type ground_gnat = ground_gnat gnat with show, gmap
@type logic_gnat = logic_gnat gnat logic with show, gmap

@type ('i, 'b, 'v, 'l) gldb =
| Iconst_ of 'i
| Bconst_ of 'b
| Var_ of 'v
| Plus_ of 'l * 'l
| Mult_ of 'l * 'l
| Equal_ of 'l * 'l
| Less_ of 'l * 'l
| If_ of 'l * 'l * 'l
| Let_ of 'l * 'l
with show, gmap

@type ground_gldb = (ground_gnat, Std.Bool.ground, ground_gnat, ground_gldb) gldb with show, gmap
@type logic_gldb  = (logic_gnat,  Std.Bool.logic,  logic_gnat,  logic_gldb) gldb logic with show, gmap

@type type_ =
| Int
| Bool

module Gnat = Fmap (struct
                      type 'a t = 'a gnat
                      let fmap f = gmap(gnat) f
                    end)

module Gldb = Fmap4 (struct
                       type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) gldb
                       let fmap f g h i = gmap(gldb) f g h i
                     end)

let rec o () = inj (Gnat.distrib O)
and s x = inj (Gnat.distrib (S x))

let rec iconst_ x = inj (Gldb.distrib (Iconst_ x))
and bconst_ x = inj (Gldb.distrib (Bconst_ x))
and var_ x = inj (Gldb.distrib (Var_ x))
and plus_ x y = inj (Gldb.distrib (Plus_ (x, y)))
and mult_ x y = inj (Gldb.distrib (Mult_ (x, y)))
and equal_ x y = inj (Gldb.distrib (Equal_ (x, y)))
and less_ x y = inj (Gldb.distrib (Less_ (x, y)))
and if_ x y z = inj (Gldb.distrib (If_ (x, y, z)))
and let_ x y = inj (Gldb.distrib (Let_ (x, y)))

let int () = !! Int
let bool () = !! Bool

let rec reify_nat t = Gnat.reify reify_nat t
let nat_to_string t = (show(logic_gnat)) (t#reify reify_nat)

let rec reify_ldb t = Gldb.reify reify_nat Std.Bool.reify reify_nat reify_ldb t
let ldb_to_string t = (show(logic_gldb)) (t#reify reify_ldb)

let run_formula n textRepr r =
  Printf.printf "-----------------------------\n%s\n" textRepr;
  List.iter (fun fm -> Printf.printf "%s\n" (ldb_to_string fm)) @@ RStream.take ~n:n @@
            r (fun fm -> fm)




