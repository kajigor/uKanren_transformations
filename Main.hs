module Main where
import MuKanren
import Driver
import Debug.Trace

-- tests
test0 = eval (call_fresh (\q -> q === at 5)) empty_state

test1 = eval a_and_b empty_state
  where a_and_b = (call_fresh (\a -> a === at 7)) 
              &&& (call_fresh (\b -> (b === at 5) ||| (b === at 6)))  

fives x = fun "fives" $ (x === at 5) ||| (fives x) 
sixes x = (x === at 6) ||| (sixes x)
test2 = eval (call_fresh fives) empty_state

test3 = eval (call_fresh (\x -> fives x ||| sixes x)) empty_state

loop q = (q === at 0) ||| (q === at 1) ||| (loop q)
test4 = eval (call_fresh (\q -> loop q)) empty_state

appendo xs ys zs = 
  fun "appendo" $
    conde [ [xs === nil, zs === ys]
--          , [appendo xs ys zs]
          , [call_fresh 
              (\h -> call_fresh 
                (\t -> 
                  (xs === pair h t) 
                  &&& (call_fresh (\r -> (zs === pair h r) 
                                     &&& (appendo t ys r)))
                )
              )
            ]
          ]


test5 = 
  let xs = list [at 1, at 2] 
      ys = list [at 3, at 4, at 5]
  in eval (call_fresh (\q -> appendo xs ys q)) empty_state
  
test6 = 
  let xs = list [at 1, at 2]
      zs = list [at 1, at 2, at 3, at 4, at 5]
  in eval (call_fresh (\q -> appendo xs q zs)) empty_state

test7 = 
  let zs = list [at 1, at 2, at 3, at 4, at 5]
  in eval (call_fresh (\q -> call_fresh (\p -> appendo q p zs))) empty_state
  
test8 = 
 {- unfold $ -} appendo (list [at 1, at 2]) (list [at 3, at 4, at 5]) (Var 0)

test9 = 
  let fivesRev_ x = zzz (fivesRev_ x) ||| (x === at 5) in
  run 5 (call_fresh (\q -> fivesRev_ q))

run k a = 
  let take k (Immature s) = take k s
      take k (Mature h t) | k > 0 = Mature h $ take (k-1) t
      take k _ = Empty
  in take k $ eval a empty_state

fives' x = fun "fives" $ (x === at 5) ||| (call (fives' x) [x]) 
test10 = run 10 (call_fresh (\q -> call (fives' q) [q]))

appendo_call xs ys zs = 
  fun "appendo" $
    conde [ [xs === nil, zs === ys]
          , [call_fresh 
              (\h -> call_fresh 
                (\t -> 
                  (xs === pair h t) 
                  &&& (call_fresh (\r -> (zs === pair h r) 
                                     &&& (call (appendo_call t ys r) [t, ys, r])))
                )
              )
            ]
          ]


test11 = 
  let xs = list [at 1, at 2] 
      ys = list [at 3, at 4, at 5]
  in eval (call_fresh (\q -> (call (appendo_call xs ys q) [xs, ys, q]))) empty_state
  
test12 = 
  let xs = list [at 1, at 2]
      zs = list [at 1, at 2, at 3, at 4, at 5]
  in eval (call_fresh (\q -> (call (appendo_call xs q zs) [xs, q, zs]))) empty_state

test13 = 
  let zs = list [at 1, at 2, at 3, at 4, at 5]
  in eval (call_fresh (\q -> call_fresh (\p -> (call (appendo_call q p zs) [q, p, zs])))) empty_state

main = 
  do 
    putStrLn $ show (drive (call_fresh (\xs -> (call_fresh (\ys -> (call_fresh (\zs -> appendo_call xs ys zs)))))) (Just empty_state))
  
    {- putStrLn "\nTest 5\n"
    putStrLn $ show test5
    putStrLn "\nTest 6\n"
    putStrLn $ show test6
    putStrLn "\nTest 7\n"
    putStrLn $ show test7

    putStrLn $ show (reify' (Var 0) test0)
    putStrLn $ show (reify' (Var 0) test1)
    putStrLn $ show (reify' (Var 0) test7)
    
    --(5 x (#t y x) z) 
    
    putStrLn $ show (reify (pair (at 5) (pair (var 0) (pair (at 1) (pair (list [var 1, var 0]) (var 2))))) empty_subst) 
   
    putStrLn $ show (test0) 
   -- line <- getLine
   -- putStrLn $ show test2
   -- putStrLn $ show test3
-}
   -- putStrLn $ show' (appendo_call (var 0) (var 1) (var 2)) 3
  
    -- putStrLn $ show (reify' (Var 0) test9)
    {- 
    putStrLn $ show test10
    putStrLn "==========================="
    putStrLn $ show (reify' (Var 0) test11)
    putStrLn "==========================="
    putStrLn $ show (reify' (Var 0) test12)
    putStrLn "==========================="
    putStrLn $ show (reify' (Var 0) test13)
    -}
