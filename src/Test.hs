module Test where

import Syntax
import Stream
import Eval
import Driving
import Tree
import TreePrinter
import Num
import Prelude hiding (succ)
import qualified Debug.Trace as T 

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
    ) $ appendo g

revAcco g =
  let xs = V "xs" 
      acc = V "acc" 
      sx = V "sx" 
      h = V "h"
      t = V "t"
  in 
  Let
    (def "revacco" ["xs", "acc", "sx"] 
       (
         (xs === nil &&& sx === acc) |||
         (fresh ["h", "t"]
           (xs === h % t) &&&
           call "revacco" [t, h % acc, sx]
         )
       )
    ) g

toplevel n printer g =
  map (\s -> printer $ reify s (V 0)) $ takeS n $ (run g)

main = 
  do
{-
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [zero, zero, V "q"])))
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [zero, V "q", zero])))
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [V "q", zero, zero])))
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [succ $ succ zero, zero, V "q"])))
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [zero, succ $ succ zero, V "q"])))
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [zero, V "q", V "q"])))
--    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [succ zero, V "q", V "q"])))
--    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [V "q", succ zero, V "q"])))
-}

{-
--    putStrLn $ show (fresh ["q"] (call "appendo" [ V "q", i "B" % nil, i "A" % (i "B" % nil)]))
    putStrLn $ show (toplevel 1 show (appendo $ fresh ["q"] (call "appendo" [nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 show (appendo $ fresh ["q"] (call "appendo" [a % nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 show (appendo $ fresh ["q"] (call "appendo" [a % b % nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 show (appendo $ fresh ["q"] (call "appendo" [ V "q", b % nil, a % b % nil])))

    putStrLn $ show (toplevel 1 show (reverso $ fresh ["q"] (call "reverso" [nil, V "q"])))
    putStrLn $ show (toplevel 1 show (reverso $ fresh ["q"] (call "reverso" [a % nil, V "q"])))
    putStrLn $ show (toplevel 1 show (reverso $ fresh ["q"] (call "reverso" [a % b % nil, V "q"])))
    putStrLn $ show (toplevel 1 show (reverso $ fresh ["q"] (call "reverso" [V "q", a % b % nil])))
-}
{---    putStrLn $ show $ drive     ([appendo'], fresh ["q"] (call "appendo'" [nil, nil, V "q"]))    
--    printTree "appendo.dot" $ drive     ([appendo'], fresh ["q"] (call "appendo'" [nil, nil, V "q"]))
-}    


    let (_, t) = drive (appendo $ 
                          fresh ["q", "r", "s", "t", "p"] 
                             (call "appendo" [V "q", V "r", V "s"] &&& 
                              call "appendo" [V "s", V "t", V "p"]
                             )
                        )
    printTree "doubleapp.dot" t

{-
    printTree "appapp.dot"  $ 
      drive ([appendo], 
              fresh ["q", "r", "s", "t", "p"] 
                 (call "appendo" [V "q", V "r", V "s"] &&& 
                  call "appendo" [V "s", V "t", V "p"]
                 )
             )
-}

    let (_, t') = drive (reverso $ fresh ["q", "r", "s"] (call "reverso" [V "q", V "r", V "s"]))
    printTree "reverso.dot" t'

