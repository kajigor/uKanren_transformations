module Main where
import MuKanren
import Driver
import Debug.Trace
import Programs


run k a =
  let
      take k (Immature s) | k > 0 = take k s
      take k (Mature h t) | k > 0 = Mature h $ take (k-1) t
      take k _ = Empty
  in take k $ eval a emptyState

main =
  do
--    print $ drive (call_fresh (\xs -> call_fresh (\ys -> (call_fresh (\zs -> call (appendo xs ys zs) [xs, ys, zs])))))

--    let l = list [at 239, at (-91)]
    print $ drive (callFresh (\xs -> callFresh (\sx -> call (reverso xs sx) [xs, sx])))
    {- let forward  l = run 1 (call_fresh (\xs -> call (reverso l xs) [l, xs]))
        backward l = run 2 (call_fresh (\xs -> call (reverso xs l) [xs, l]))
        empty = list []
        singleton = list [at 239]
        three = list [at 239, at 13, at (-91)]
        runtest t = do putStrLn $ "States stream: " ++ show t
                       print (reify' (Var 0) t)
    runtest (forward empty)
    runtest (forward singleton)
    runtest (forward three)
    runtest (forward empty)
    runtest (forward singleton)
    runtest (forward three)
    runtest (run 5 (call_fresh (\q -> call_fresh (\x -> (call_fresh (\y ->
      (q === pair x y) &&&
      call (appendo x x y) [x, x, y]))))))
    -}

    {-
    putStrLn "Forwards direction"
    -- An attempt to get more than one answer here leads to nontermination.
    -- The reason is that (appendo a' [239] b) produces infinite stream of states which
    -- are then bound to (reverso [] a') which refute every state except the one with a' === [],
    -- but to find that there is no "second" answer, we need to check every one of the states.
    let r' = run 1 (call_fresh (\xs -> call (reverso (list [at 239]) xs) [list [at 239], xs]))
    putStrLn $ "States stream: " ++ show r'
    putStrLn $ "Reified: " ++ show (reify' (Var 0) r')

    putStrLn "\nBackwards direction"
    -- Here, all the answers can be calculated.
    let r  = run 2 (call_fresh (\xs -> call (reverso xs (list [at 239])) [xs, list [at 239]]))
    putStrLn $ "States stream: " ++ show r
    putStrLn $ "Reified: " ++  show (reify' (Var 0) r)
    -}


