open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec fail () = fail 
  and ando x y b = (((x === !!true) &&& (y === !!true) &&& (b === !!true)) ||| ((x === !!false) &&& (y === !!true) &&& (b === !!false)) ||| ((x === !!true) &&& (y === !!false) &&& (b === !!false)) ||| ((x === !!false) &&& (y === !!false) &&& (b === !!false))) 
  and oro x y b = (((x === !!true) &&& (y === !!true) &&& (b === !!true)) ||| ((x === !!false) &&& (y === !!true) &&& (b === !!true)) ||| ((x === !!true) &&& (y === !!false) &&& (b === !!true)) ||| ((x === !!false) &&& (y === !!false) &&& (b === !!false))) 
  and noto x b = (((x === !!true) &&& (b === !!false)) ||| ((x === !!false) &&& (b === !!true))) 
  and implicationo x y b = (((x === !!false) &&& (y === !!true) &&& (b === !!true)) ||| ((x === !!false) &&& (y === !!false) &&& (b === !!true)) ||| ((x === !!true) &&& (y === !!true) &&& (b === !!true)) ||| ((x === !!true) &&& (y === !!false) &&& (b === !!false))) 
  and evalo st fm u = (fresh (x y v w z) (((fm === (lit u)) ||| ((fm === (var z)) &&& (elemo z st u)) ||| ((oro v w u) &&& (evalo st x v) &&& (evalo st y w) &&& (fm === (disj x y))) ||| ((ando v w u) &&& (evalo st x v) &&& (evalo st y w) &&& (fm === (conj x y)))))) 
  and elemo n s v = (fresh (h t n') ((((n === (zero ())) &&& (s === (h % t)) &&& (v === h)) ||| ((s === (h % t)) &&& (elemo n' t v) &&& (n === (succ n')))))) 
  in        (fresh (x) ((evalo ((List.nil ())) x !!true)))
