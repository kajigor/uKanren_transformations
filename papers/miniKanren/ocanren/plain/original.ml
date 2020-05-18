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

let ando x y r =
  conde [
    (x === !!true ) &&& (y === !!true ) &&& (r === !!true);
    (x === !!true ) &&& (y === !!false) &&& (r === !!false);
    (x === !!false) &&& (y === !!true ) &&& (r === !!false);
    (x === !!false) &&& (y === !!false) &&& (r === !!false)
  ]

let oro x y r =
  conde [
    (x === !!true ) &&& (y === !!true ) &&& (r === !!true);
    (x === !!true ) &&& (y === !!false) &&& (r === !!true);
    (x === !!false) &&& (y === !!true ) &&& (r === !!true);
    (x === !!false) &&& (y === !!false) &&& (r === !!false)
  ]

let noto x r =
  conde [
    (x === !!true)  &&& (r === !!false);
    (x === !!false) &&& (r === !!true)
  ]


let rec evalo st fm u =
  fresh (x y z v w) (
    conde [
      fm === conj x y
         &&& ando v w u
         &&& evalo st x v
         &&& evalo st y w;
      fm === disj x y
         &&& oro  v w u
         &&& evalo st x v
         &&& evalo st y w;
      fm === neg  x
         &&& noto v u
         &&& evalo st x v;
      fm === var  z
         &&& elemo st z u
  ])

let topLevel st fm = evalo st fm (!!true)