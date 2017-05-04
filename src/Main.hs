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
  in take k $ eval a empty_state

main =
  do
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


