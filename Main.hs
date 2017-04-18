module Main where
import MuKanren
import Driver

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

main = 
  do 
    putStrLn $ show test0
    putStrLn $ show test1
   -- putStrLn $ show test4
   -- line <- getLine
   -- putStrLn $ show test2
   -- putStrLn $ show test3
  
