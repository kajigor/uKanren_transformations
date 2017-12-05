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

appendo g =
  let x  = V "x"  in
  let y  = V "y"  in
  let xy = V "xy" in
  let h  = V "h"  in
  let t  = V "t"  in
  let ty = V "ty" in
  Let
    (def "appendo" ["x", "y", "xy"] 
         ((x === nil &&& xy === y) ||| 
          (fresh ["h", "t", "ty"] 
             (x  === h % t  &&&
              xy === h % ty &&&
              call "appendo" [t, y, ty]
             )
          )
         )
    ) g

appendo' g =
  let x  = V "x"  in
  let y  = V "y"  in
  let xy = V "xy" in
  let h  = V "h"  in
  let t  = V "t"  in
  let ty = V "ty" in
  Let 
    (def "appendo'" ["x", "y", "xy"] 
           ((x === nil ||| xy === y) ||| 
            (fresh ["h", "t", "ty"] 
               (x  === h % t  |||
                xy === h % ty |||
                call "appendo'" [t, y, ty]
               )
            )
           )
    ) g

reverso g =
  let x  = V "x"  in
  let y  = V "y"  in
  let h  = V "h"  in
  let t  = V "t"  in
  let rt = V "rt" in
  Let 
    (def "reverso" ["x", "y"]
           ((x === nil &&& y === nil) |||
            (fresh ["h", "t", "rt"]
               (x === h % t &&&
                call "reverso" [t, rt] &&&
                call "appendo" [rt, h % nil, y]
               )
            )
           )
    ) g

toplevel n spec = map (\s -> list $ reify s (V 0)) $ takeS n $ (run spec)

{-
main = 
  do
    putStrLn $ show (fresh ["q"] (call "appendo" [ V "q", i "B" % nil, i "A" % (i "B" % nil)]))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [a % nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [a % b % nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [ V "q", b % nil, a % b % nil])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [a % nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [a % b % nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [V "q", a % b % nil])))
--    putStrLn $ show $ drive     ([appendo'], fresh ["q"] (call "appendo'" [nil, nil, V "q"]))    
--    printTree "appendo.dot" $ drive     ([appendo'], fresh ["q"] (call "appendo'" [nil, nil, V "q"]))    

    printTree "appapp.dot"  $ 
      drive ([appendo], 
              fresh ["q", "r", "s", "t", "p"] 
                 (call "appendo" [V "q", V "r", V "s"] &&& 
                  call "appendo" [V "s", V "t", V "p"]
                 )
             )


    printTree "reverso.dot"  $ 
      drive ([reverso, appendo], 
              fresh ["q", "r", "s"] 
                 (call "reverso" [V "q", V "r", V "s"]
                 )
             )
-}