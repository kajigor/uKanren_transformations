module Functional = struct
  open L

  let print_type = function
  | Integer -> "integer"
  | Boolean -> "boolean"

  let print_option printer = function
  | Some x -> printer x
  | None -> "None"

  let rec to_int = function
  | O -> 0
  | S x -> 1 + to_int x

  let rec to_nat = function
  | 0 -> O
  | n -> S (to_nat (n - 1))

  let string_of_nat x =
    let i = to_int x in
    string_of_int i

  let rec print_fm = function
  | Iconst_ x -> string_of_int x
  | Bconst_ x -> string_of_bool x
  | Var_ v -> Printf.sprintf "_.%s" (string_of_nat v)
  | Plus_ (x, y) -> Printf.sprintf "(%s + %s)" (print_fm x) (print_fm y)
  | Mult_ (x, y) -> Printf.sprintf "(%s * %s)" (print_fm x) (print_fm y)
  | Equal_ (x, y) -> Printf.sprintf "(%s == %s)" (print_fm x) (print_fm y)
  | Less_ (x, y) -> Printf.sprintf "(%s < %s)" (print_fm x) (print_fm y)
  | If_ (c, t, e) -> Printf.sprintf "if %s then %s else %s" (print_fm c) (print_fm t) (print_fm e)
  | Let_ (v, b) -> Printf.sprintf "let %s in\n%s" (print_fm v) (print_fm b)

  let test x =
    let type_ = typecheck_ [] x in
    Printf.printf "============================\n%s\n" (print_fm x);
    Printf.printf "%s\n%!" @@ print_option print_type type_

  let _ =
    (* test (Plus_ (Iconst_ 13, Iconst_ 42)) ;
    test (Plus_ (Iconst_ 13, Bconst_ true)) ;
    test (Plus_ (Bconst_ false, Iconst_ 42)) ;
    test (Plus_ (Bconst_ false, Bconst_ true));
    test (Less_ (Iconst_ 13, Iconst_ 42)) ;
    test (Less_ (Iconst_ 13, Bconst_ true)) ;
    test (Less_ (Bconst_ false, Iconst_ 42)) ;
    test (Less_ (Bconst_ false, Bconst_ true));
    test (Equal_ (Iconst_ 13, Iconst_ 42)) ;
    test (Equal_ (Iconst_ 13, Bconst_ true)) ;
    test (Equal_ (Bconst_ false, Iconst_ 42)) ;
    test (Equal_ (Bconst_ false, Bconst_ true)) ; *)
    test (Let_ (Iconst_ 13, Let_ (Iconst_ 42, Let_ (Plus_ (Var_ (to_nat 1), Var_ (to_nat 0)), If_ (Less_ (Var_ (to_nat 0), Iconst_ 100), Equal_ (Var_ (to_nat 0), Var_ (to_nat 2)), Bconst_ false)))))
end

module Relational = struct
  open Helper
  open L_ocanren
  open OCanren
  open OCanren.Std

  let _ =
    let name = "typecheck [] fm (Some Int)" in
    let transformer fm = typecheck_ (ocanren ([])) fm (some (integer ())) in
    run_formula 100 name @@
    run q (fun fm -> transformer fm)

  end

module Time = struct
  open OCanren
  open GT
  open Helper

  let inputs = [ ("original", Original.topLevel)
               ; ("trans", Llang.topLevel)
               ; ("ecce", Ecce.topLevel)
               ; ("peter", L_ocanren.topLevel)
               ; ("eccePeter", EccePeter.topLevel)
               ; ("transPeter", LlangPeter.topLevel)
               ]

  let _ =
    do_tables 100 (fun transformer -> run q (fun fm -> transformer fm)) inputs
end