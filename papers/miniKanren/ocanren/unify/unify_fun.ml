type nat  = Z | S of nat
type term = Var_ of nat | Constr of nat * term list

let rec eq_nat a b =
  match a, b with
  | Z,   Z   -> true
  | S _, Z   -> false
  | Z,   S _ -> false
  | S x, S y -> eq_nat x y


let rec get_term var subst =
  match subst with
  | []    -> None
  | x::xs ->
    match var with
    | Z   -> x
    | S n -> get_term n xs


let rec check_uni subst t1 t2 =
  let rec forall2 subst l1 l2 =
    match l1, l2 with
    | []   , []    -> true
    | x::xs, y::ys -> check_uni subst x y && forall2 subst xs ys in

  match t1, t2 with
  | Constr (n1, a1), Constr (n2, a2) ->
      eq_nat n1 n2 && forall2 subst a1 a2
  | Var_ v         , Constr (n, a)   ->
    begin match get_term v subst with
    | None   -> false
    | Some t -> check_uni subst t t2
    end
  | Constr (n, a)  , Var_ v          ->
    begin match get_term v subst with
    | None   -> false
    | Some t -> check_uni subst t1 t
    end
  | Var_ v1        , Var_ v2         ->
    match get_term v1 subst with
    | Some t1' -> check_uni subst t1' t2
    | None     ->
      match get_term v2 subst with
      | Some _ -> false
      | None   -> eq_nat v1 v2
