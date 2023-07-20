open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec fail () = fail 
  and ando x y b = (((x === (trueo ())) &&& (y === (trueo ())) &&& (b === (trueo ()))) ||| ((x === (falso ())) &&& (y === (trueo ())) &&& (b === (falso ()))) ||| ((x === (trueo ())) &&& (y === (falso ())) &&& (b === (falso ()))) ||| ((x === (falso ())) &&& (y === (falso ())) &&& (b === (falso ())))) 
  and oro x y b = (((x === (trueo ())) &&& (y === (trueo ())) &&& (b === (trueo ()))) ||| ((x === (falso ())) &&& (y === (trueo ())) &&& (b === (trueo ()))) ||| ((x === (trueo ())) &&& (y === (falso ())) &&& (b === (trueo ()))) ||| ((x === (falso ())) &&& (y === (falso ())) &&& (b === (falso ())))) 
  and noto x b = (((x === (trueo ())) &&& (b === (falso ()))) ||| ((x === (falso ())) &&& (b === (trueo ())))) 
  and implicationo x y b = (((x === (falso ())) &&& (y === (trueo ())) &&& (b === (trueo ()))) ||| ((x === (falso ())) &&& (y === (falso ())) &&& (b === (trueo ()))) ||| ((x === (trueo ())) &&& (y === (trueo ())) &&& (b === (trueo ()))) ||| ((x === (trueo ())) &&& (y === (falso ())) &&& (b === (falso ())))) 
  and evalo st fm u = (fresh (x y v w z) (((fm === (lit u)) ||| ((fm === (var z)) &&& (elemo z st u)) ||| ((noto v u) &&& (evalo st x v) &&& (fm === (neg x))) ||| ((oro v w u) &&& (evalo st x v) &&& (evalo st y w) &&& (fm === (disj x y))) ||| ((ando v w u) &&& (evalo st x v) &&& (evalo st y w) &&& (fm === (conj x y))) ||| ((implicationo v w u) &&& (evalo st x v) &&& (evalo st y w) &&& (fm === (impl x y)))))) 
  and elemo n s v = (fresh (h t n') ((((n === (zero ())) &&& (s === (h % t)) &&& (v === h)) ||| ((s === (h % t)) &&& (elemo n' t v) &&& (n === (succ n')))))) 
  in        (fresh (x) ((evalo ((List.nil ())) ((disj x x)) ((trueo ())))))
