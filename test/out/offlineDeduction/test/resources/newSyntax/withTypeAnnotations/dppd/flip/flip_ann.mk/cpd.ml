open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec flipflip y0 y1 = (fresh (q1 q2 q3 q4 q5 q6) ((((y1 === (leaf q1)) &&& (y0 === (leaf q1))) ||| ((y1 === (tree q2 q3 q4)) &&& (_flipFlip q5 q4) &&& (y0 === (tree q6 q3 q5)) &&& (_flipFlip q6 q2))))) 
  and _flipFlip y5 y7 = (fresh (q1 q2 q3 q4 q5 q6) ((((y7 === (leaf q1)) &&& (y5 === (leaf q1))) ||| ((y7 === (tree q2 q3 q4)) &&& (_flipFlip q5 q4) &&& (y5 === (tree q6 q3 q5)) &&& (_flipFlip q6 q2))))) 
  in   (flipflip x0 x1)
