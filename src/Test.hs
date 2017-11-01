module Test where

import Syntax
import Stream
import Eval
--import Driving
--import Tree

-- Tests
infixr 9 %

nil   = C "Nil"  []     
x % y = C "Cons" [x, y] 
i x   = C x      []

a = i "a"
b = i "b"
c = i "c"
d = i "d"

list (V n) = "._" ++ show n
list (C "Cons" [h, t]) = list h ++ " % " ++ list t
list (C "Nil"  _     ) = "nil"
list (C s []) = s

reify s x@(V v) =
  case lookup v s of
    Nothing -> x
    Just t  -> reify s t
reify s (C n ts) = C n $ map (reify s) ts

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
             (x  === h % t  &&&
              xy === h % ty &&&
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
             (x  === h % t  |||
              xy === h % ty |||
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
             (x === h % t &&&
              call "appendo" [rt, h % nil, y] &&&
              call "reverso" [t, rt] 
             )
          )
         )

toplevel n spec = map (\s -> list $ reify s (V 0)) $ takeS n $ (run spec)

{-
main = 
  do
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [a % nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [a % b % nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [ V "q", b % nil, a % b % nil])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [a % nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [a % b % nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [V "q", a % b % nil])))
--    putStrLn $ show $ drive     ([appendo'], fresh ["q"] (call "appendo'" [nil, nil, V "q"]))    
    printTree "appendo.dot" $ drive     ([appendo'], fresh ["q"] (call "appendo'" [nil, nil, V "q"]))    
-}