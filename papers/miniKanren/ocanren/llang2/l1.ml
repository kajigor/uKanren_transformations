type ldb = Iconst_ of int
         | Bconst_ of bool
         | Var_ of int
         | Plus_ of ldb * ldb
         | Mult_ of ldb * ldb
         | Equal_ of ldb * ldb
         | Less_ of ldb * ldb
         | If_ of ldb * ldb * ldb
         | Let_ of ldb * ldb

type type_ = Int | Bool

let (>>=) e f =
  match e with
  | None -> None
  | Some x -> f x
(*
let chainIfEqual e v f =
  match e with
  | Some x when x == v -> f v
  | _ -> None
*) 

let chainIfEqual e v f =
  if Some v == e then f v else None 


let rec typecheck_ gamma term =
  match term with
  | Iconst_ _ -> Some Int
  | Bconst_ _ -> Some Bool
  | Var_ v -> List.nth_opt gamma v
  | Plus_ (x, y) ->
      chainIfEqual (typecheck_ gamma x) Int @@ fun k ->
      chainIfEqual (typecheck_ gamma y) Int @@ fun l ->
      Some Int
  | Mult_ (x, y) ->
      chainIfEqual (typecheck_ gamma x) Int @@ fun k ->
      chainIfEqual (typecheck_ gamma y) Int @@ fun l ->
      Some Int
  | Equal_ (x, y) ->
      typecheck_ gamma x >>= fun x' ->
      typecheck_ gamma y >>= fun y' ->
      if x' == y'
      then Some Bool
      else None
  | Less_ (x, y) ->
      chainIfEqual (typecheck_ gamma x) Int @@ fun k ->
      chainIfEqual (typecheck_ gamma y) Int @@ fun l ->
      Some Bool
  | If_ (c, t, e) ->
      chainIfEqual (typecheck_ gamma c) Bool @@ fun l ->
      typecheck_ gamma t >>= fun t' ->
      typecheck_ gamma e >>= fun e' ->
      if t' == e'
      then Some t'
      else None
  | Let_ (v, b) ->
      typecheck_ gamma v >>= fun v' ->
      typecheck_ (v' :: gamma) b

