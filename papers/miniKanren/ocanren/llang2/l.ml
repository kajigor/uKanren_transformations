type nat = O | S of nat

type ldb = Iconst_ of int
         | Bconst_ of bool
         | Var_ of nat
         | Plus_ of ldb * ldb
         | Mult_ of ldb * ldb
         | Equal_ of ldb * ldb
         | Less_ of ldb * ldb
         | If_ of ldb * ldb * ldb
         | Let_ of ldb * ldb

type type_ = Int | Bool

let type_eq x y =
  match (x, y) with
  | (Int, Int) -> true
  | (Bool, Bool) -> true
  | (Int, Bool) -> false
  | (Bool, Int) -> false

let rec nth_opt xs n =
  match xs with
  | [] -> None
  | (h :: t) ->
    match n with
    | O -> Some h
    | S x -> nth_opt t x

let rec typecheck_ gamma term =
  match term with
  | Iconst_ _ -> Some Int
  | Bconst_ _ -> Some Bool
  | Var_ v -> nth_opt gamma v
  | Plus_ (x, y) -> (
      match typecheck_ gamma x with
      | None -> None
      | Some x' ->
        match typecheck_ gamma y with
        | None -> None
        | Some y' ->
          if type_eq x' Int && type_eq y' Int
          then Some Int
          else None
      )
  | Mult_ (x, y) -> (
      match typecheck_ gamma x with
      | None -> None
      | Some x' ->
        match typecheck_ gamma y with
        | None -> None
        | Some y' ->
          if type_eq x' Int && type_eq y' Int
          then Some Int
          else None
      )
  | Equal_ (x, y) -> (
      match typecheck_ gamma x with
      | None -> None
      | Some x' ->
        match typecheck_ gamma y with
        | None -> None
        | Some y' ->
          if type_eq x' y'
          then Some Bool
          else None
      )
  | Less_ (x, y) -> (
      match typecheck_ gamma x with
      | None -> None
      | Some x' ->
        match typecheck_ gamma y with
        | None -> None
        | Some y' ->
          if type_eq x' Int && type_eq y' Int
          then Some Bool
          else None
      )
  | If_ (c, t, e) -> (
      match typecheck_ gamma c with
      | None -> None
      | Some c' ->
        if type_eq c' Bool
        then
          match typecheck_ gamma t with
          | None -> None
          | Some t' ->
            match typecheck_ gamma e with
            | None -> None
            | Some e' ->
              if type_eq t' e'
              then Some t'
              else None
        else None
      )
  | Let_ (v, b) -> (
      match typecheck_ gamma v with
      | None -> None
      | Some v' ->
        typecheck_ (v' :: gamma) b
      )
