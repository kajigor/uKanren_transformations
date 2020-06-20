module Main where 

import Data.List (intercalate)

createList :: Int -> (Int -> [Int]) -> [(Int, [Int])]
createList num f = 
  [(x, f x) | x <- [1 .. num]]

showCreatedList :: [(Int, [Int])] -> [(String, String)]
showCreatedList = 
  map (\(x, y) -> (show x, "ocanren ([" ++ (intercalate "; " $ map show y) ++ "])"))

showCreatedPair :: (String, String) -> IO () 
showCreatedPair (x, y) = do 
  putStr "("
  putStr (show x)
  putStr ", "
  putStr y 
  putStr ")"

putList :: [(Int, [Int])] -> IO () 
putList lst = do  
  let (h:t) = showCreatedList lst 
  putStr "[ " 
  showCreatedPair h
  mapM (\pair -> do 
           putStr "\n; "
           showCreatedPair pair
       ) t
  putStrLn "\n]"

main :: IO () 
main = do 
  putList $ createList 10 (\x -> [1 .. x * 10])
  putList $ createList 10 (\x -> take (x * 100) (repeat 0))




