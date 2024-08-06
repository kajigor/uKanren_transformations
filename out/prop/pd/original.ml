open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec evalo st fm u = (fresh (x y v w var) ((((ando v w u) &&& ((evalo st x v) &&& ((evalo st y w) &&& (fm === (conj x y))))) ||| (((oro v w u) &&& ((evalo st x v) &&& ((evalo st y w) &&& (fm === (disj x y))))) ||| (((noto v u) &&& ((evalo st x v) &&& (fm === (neg x)))) ||| (((fm === (var var)) &&& (assoco var st u)) ||| (fm === (lit u)))))))) 
  and ando a b c = (fresh (ab) (((nando a b ab) &&& (nando ab ab c)))) 
  and nando a b c = (((a === !!false) &&& ((b === !!false) &&& (c === !!true))) ||| (((a === !!false) &&& ((b === !!true) &&& (c === !!true))) ||| (((a === !!true) &&& ((b === !!false) &&& (c === !!true))) ||| ((a === !!true) &&& ((b === !!true) &&& (c === !!false)))))) 
  and oro a b c = (fresh (aa bb) (((nando a a aa) &&& ((nando b b bb) &&& (nando aa bb c))))) 
  and nando a b c = (((a === !!false) &&& ((b === !!false) &&& (c === !!true))) ||| (((a === !!false) &&& ((b === !!true) &&& (c === !!true))) ||| (((a === !!true) &&& ((b === !!false) &&& (c === !!true))) ||| ((a === !!true) &&& ((b === !!true) &&& (c === !!false)))))) 
  and noto a na = (nando a a na) 
  and nando a b c = (((a === !!false) &&& ((b === !!false) &&& (c === !!true))) ||| (((a === !!false) &&& ((b === !!true) &&& (c === !!true))) ||| (((a === !!true) &&& ((b === !!false) &&& (c === !!true))) ||| ((a === !!true) &&& ((b === !!true) &&& (c === !!false)))))) 
  and assoco x xs v = (fresh (a b tl) (((xs === (((pair a b)) % tl)) &&& (((a === x) &&& (b === v)) ||| (assoco x tl v))))) 
  in         (fresh (fm st) ((evalo st fm !!true)))
