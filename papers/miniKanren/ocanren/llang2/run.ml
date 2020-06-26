open L

let print_type = function
| Int -> "int"
| Bool -> "bool"

let print_option printer = function
| Some x -> printer x
| None -> "None"

let rec print_fm = function
| Iconst_ x -> string_of_int x
| Bconst_ x -> string_of_bool x
| Var_ v -> Printf.sprintf "_.%s" (string_of_int v)
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
  test (Plus_ (Iconst_ 13, Iconst_ 42)) ;
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
  test (Equal_ (Bconst_ false, Bconst_ true)) ;
  test (Let_ (Iconst_ 13, Let_ (Iconst_ 42, Let_ (Plus_ (Var_ 1, Var_ 0), If_ (Less_ (Var_ 0, Iconst_ 100), Equal_ (Var_ 0, Var_ 2), Bconst_ false)))))

