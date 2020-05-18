open OCanren
open GT
open Helper

let rec elemo xs n v =
  fresh (t n1 h) (
    conde [
      n === Std.Nat.zero &&& (xs === Std.( % ) h t) &&& (h === v);
      n === Std.Nat.succ n1 &&& (xs === Std.( % ) h t) &&& (elemo t n1 v)
    ]
  )

let rec evalo st fm u =
  fresh (x y z v w) (
    conde [
      fm === conj x y &&& evalo st x v
          &&& evalo st y w &&& Std.Bool.ando v w u;
      fm === disj x y &&& evalo st x v
          &&& evalo st y w &&& Std.Bool.oro  v w u;
      fm === neg  x   &&& evalo st x v
          &&& Std.Bool.noto v u ;
      fm === var  z   &&& elemo st z u
  ])

let topLevel st fm = evalo st fm (!!true)
