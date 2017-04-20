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

main = 
  do 
    putStrLn "\nTest 5\n"
    putStrLn $ show test5
    putStrLn "\nTest 6\n"
    putStrLn $ show test6
   -- putStrLn $ show test1
   -- putStrLn $ show test4
   -- line <- getLine
   -- putStrLn $ show test2
   -- putStrLn $ show test3
  
