module Main where
import MuKanren
import Driver
import Debug.Trace
import Programs


run k a = 
  let take k (Immature s) = take k s
      take k (Mature h t) | k > 0 = Mature h $ take (k-1) t
      take k _ = Empty
  in take k $ eval a empty_state

main = 
  do 
    putStrLn "Forwards direction"
    let r' = run 1 (call_fresh (\xs -> call (reverso (list [at 239]) xs) [(list [at 239]), xs]))
    putStrLn $ "States stream: " ++ show r'
    putStrLn $ "Reified: " ++ show (reify' (Var 0) (r'))

    putStrLn "Press any key to run the program backwards"
    getLine

    putStrLn "\nBackwards direction"
    let r  = run 1 (call_fresh (\xs -> call (reverso xs (list [at 239])) [xs, (list [at 239])]))
    putStrLn $ "States stream: " ++ show r
    putStrLn $ "Reified: " ++  show (reify' (Var 0) (r))



