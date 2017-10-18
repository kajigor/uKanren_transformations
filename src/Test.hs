module Test where

import Syntax
import Stream
import Eval

-- Tests
nil      = C "Nil"  []     
cons x y = C "Cons" [x, y] 
i    x   = C x      []

appendo =
  let x  = V "x"  in
  let y  = V "y"  in
  let xy = V "xy" in
  let h  = V "h"  in
  let t  = V "t"  in
  let ty = V "ty" in
  def "appendo" ["x", "y", "xy"] 
         ((x === nil &&& xy === y) ||| 
          (fresh ["h", "t", "ty"] 
             (x  === h `cons` t  &&&
              xy === h `cons` ty &&&
              call "appendo" [t, y, ty]
             )
          )
         )


appendo' =
  let x  = V "x"  in
  let y  = V "y"  in
  let xy = V "xy" in
  let h  = V "h"  in
  let t  = V "t"  in
  let ty = V "ty" in
  def "appendo'" ["x", "y", "xy"] 
         ((x === nil ||| xy === y) ||| 
          (fresh ["h", "t", "ty"] 
             (x  === h `cons` t  |||
              xy === h `cons` ty |||
              call "appendo'" [t, y, ty]
             )
          )
         )

reverso =
  let x  = V "x"  in
  let y  = V "y"  in
  let h  = V "h"  in
  let t  = V "t"  in
  let rt = V "rt" in
  def "reverso" ["x", "y"]
         ((x === nil &&& y === nil) |||
          (fresh ["h", "t", "rt"]
             (x === h `cons` t &&&
              call "appendo" [rt, h `cons` nil, y] &&&
              call "reverso" [t, rt] 
             )
          )
         )

toplevel n spec = takeS n $ (run spec)

main = 
  do
    putStrLn $ show (fresh ["q"] (call "appendo" [ V "q", i "B" `cons` nil, i "A" `cons` (i "B" `cons` nil)]))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [i "A" `cons` nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [i "A" `cons` nil, i "B" `cons` nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [ V "q", i "B" `cons` nil, i "A" `cons` (i "B" `cons` nil)])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [i "A" `cons` nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [i "A" `cons` (i "B" `cons` nil), V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [V "q", i "A" `cons` (i "B" `cons` nil)])))
    
  
