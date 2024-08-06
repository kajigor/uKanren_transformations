open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec evalo y0 = (fresh (x1 x2 x4 x3) ((success &&& ((y0 === (lit !!true)) ||| ((y0 === (disj x1 x2)) &&& (success &&& (((x4 === !!true) &&& (x3 === !!true)) ||| ((x4 === !!true) &&& (x3 === !!false)) ||| ((x4 === !!false) &&& (x3 === !!true)))) &&& (success &&& (_evalo x1 x3)) &&& (success &&& (_evalo x2 x4))) ||| ((y0 === (conj x1 x2)) &&& (success &&& (x4 === !!true) &&& (x3 === !!true)) &&& (success &&& (_evalo x1 x3)) &&& (success &&& (_evalo x2 x4))))))) 
  and _evalo y1 y2 = (fresh (x0 x2 x6 x7 x9 x8) (((x0 === (disj y1 x2)) &&& ((y1 === (lit y2)) ||| ((y1 === (disj x6 x7)) &&& (success &&& (((x9 === !!true) &&& (x8 === !!true) &&& (y2 === !!true)) ||| ((x9 === !!true) &&& (x8 === !!false) &&& (y2 === !!true)) ||| ((x9 === !!false) &&& (x8 === !!true) &&& (y2 === !!true)) ||| ((x9 === !!false) &&& (x8 === !!false) &&& (y2 === !!false)))) &&& (success &&& (_evalo x6 x8)) &&& (success &&& (_evalo x7 x9))) ||| ((y1 === (conj x6 x7)) &&& (success &&& (((x9 === !!true) &&& (x8 === !!true) &&& (y2 === !!true)) ||| ((x9 === !!true) &&& (x8 === !!false) &&& (y2 === !!false)) ||| ((x9 === !!false) &&& (x8 === !!true) &&& (y2 === !!false)) ||| ((x9 === !!false) &&& (x8 === !!false) &&& (y2 === !!false)))) &&& (success &&& (_evalo x6 x8)) &&& (success &&& (_evalo x7 x9))))))) 
  in   (evalo x0)
