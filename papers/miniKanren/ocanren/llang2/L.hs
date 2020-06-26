module Main where 

import Text.Printf 
import Data.List (lookup) 
import Control.Monad (guard)

data Ldb = Iconst_ Int 
         | Bconst_ Bool 
         | Var_ Int 
         | Plus_ Ldb Ldb
         | Mult_ Ldb Ldb
         | Equal_ Ldb Ldb
         | Less_ Ldb Ldb
         | If_ Ldb Ldb Ldb
         | Let_ Ldb Ldb

instance Show Ldb where 
  show (Iconst_ i) = show i 
  show (Bconst_ b) = show b 
  show (Var_ i) = printf "_.%s" $ show i 
  
  show (Plus_ x y) = printf "(%s + %s)" (show x) (show y)
  show (Mult_ x y) = printf "(%s * %s)" (show x) (show y)
  show (Equal_ x y) = printf "(%s == %s)" (show x) (show y)
  show (Less_ x y) = printf "(%s < %s)" (show x) (show y)
         
  show (If_ c t e) = printf "(if %s then %s else %s)" (show c) (show t) (show e)
  show (Let_ v b) = printf "let %s in\n%s" (show v) (show b)

toDeBruijn :: L -> Maybe Ldb
toDeBruijn = 
    go []
  where 
    go _ (Iconst i) = Just $ Iconst_ i
    go _ (Bconst i) = Just $ Bconst_ i
    go vs (Var v) = do 
      vi <- idx vs v 
      return $ Var_ vi 

    go vs (Plus x y) = do 
      x' <- go vs x  
      y' <- go vs y 
      return $ Plus_ x' y' 
    go vs (Mult x y) = do 
      x' <- go vs x
      y' <- go vs y
      return $ Mult_ x' y'
    go vs (Equal x y) = do 
      x' <- go vs x 
      y' <- go vs y
      return $ Equal_ x' y' 
    go vs (Less x y) = do 
      x' <- go vs x 
      y' <- go vs y 
      return $ Less_ x' y'

    go vs (Let x v b) = do 
      v' <- go vs v 
      b' <- go (x:vs) b
      return $ Let_ v' b'
    go vs (If c t e) = do 
      c' <- go vs c 
      t' <- go vs t 
      e' <- go vs e 
      return $ If_ c' t' e'
    
    idx (h:t) x | h == x = Just 0 
    idx (_:t) x = (1+) <$> idx t x 
    idx _ _ = Nothing 

toNormal :: String -> Ldb -> Maybe L
toNormal = 
    go [] 
  where 
    go _ _ (Iconst_ i) = Just $ Iconst i 
    go _ _ (Bconst_ i) = Just $ Bconst i 
    go stack _ (Var_ i) | length stack > i = Just $ Var [stack !! i]
    go stack _ (Var_ i) = Nothing 
    
    go stack vars (Plus_ x y) = do 
      x' <- go stack vars x  
      y' <- go stack vars y
      return $ Plus x' y'
    go stack vars (Mult_ x y) = do 
      x' <- go stack vars x  
      y' <- go stack vars y
      return $ Mult x' y'
    go stack vars (Equal_ x y) = do 
      x' <- go stack vars x 
      y' <- go stack vars y 
      return $ Equal x' y'
    go stack vars (Less_ x y) = do 
      x' <- go stack vars x 
      y' <- go stack vars y 
      return $ Less x' y' 
       
    go stack vars@(h:t) (Let_ v b) = do 
      v' <- go stack vars v 
      b' <- go (h:stack) t b 
      return $ Let [h] v' b' 
    go stack vars (If_ c t e) = do 
      c' <- go stack vars c 
      t' <- go stack vars t 
      e' <- go stack vars e 
      return $ If c' t' e'  

interp :: Ldb -> Maybe (Either Int Bool)
interp t = do 
    _ <- typecheck_ [] t
    return $ go [] t
  where 
    go _ (Iconst_ i) = Left i 
    go _ (Bconst_ b) = Right b 
    go vs (Var_ v) = vs !! v 
    go vs (Plus_ x y) = 
      case go vs x of 
        Left x' -> 
          case go vs y of 
            Left y' -> Left $ x' + y'  
    go vs (Mult_ x y) = 
      case go vs x of 
        Left x' -> 
          case go vs y of 
            Left y' -> Left $ x' * y'  
    go vs (Equal_ x y) = 
      case go vs x of 
        Left x' -> 
          case go vs y of 
            Left y' -> Right $ x' == y'  
        Right x' -> 
          case go vs y of 
            Right y' -> Right $ x' == y'
    go vs (Less_ x y) = 
      case go vs x of 
        Left x' -> 
          case go vs y of 
            Left y' -> Right $ x' < y'
    go vs (If_ c t e) = 
      case go vs c  of 
        Right cond | cond -> go vs t 
        Right cond -> go vs e  
    go vs (Let_ v b) = 
      go (go vs v : vs) b             

data L = Iconst Int 
       | Bconst Bool
       | Var String 
       | Plus L L 
       | Mult L L 
       | Equal L L 
       | Less L L 
       | If L L L 
       | Let String L L 

instance Show L where 
  show (Iconst i) = show i 
  show (Bconst i) = show i 
  show (Var s) = s 

  show (Plus x y) = printf "(%s + %s)" (show x) (show y)
  show (Mult x y) = printf "(%s * %s)" (show x) (show y)
  show (Equal x y) = printf "(%s == %s)" (show x) (show y)
  show (Less x y) = printf "(%s > %s)" (show x) (show y)

  show (If c t e) = printf "(if %s then %s else %s)" (show c) (show t) (show e)
  show (Let x v b) = printf "let %s = %s in\n%s" x (show v) (show b)

data Type = Tbool | Tint
          deriving (Show, Eq)

typecheck_ :: [Type] -> Ldb -> Maybe Type 
typecheck_ = 
    go 
  where 
    go _ (Iconst_ _) = Just Tint
    go _ (Bconst_ _) = Just Tbool
    go vs (Var_ v) | length vs <= v = Nothing
    go vs (Var_ v) = Just $ vs !! v
    go vs (Plus_ x y) = do 
      Tint <- go vs x
      Tint <- go vs y 
      return Tint
    go vs (Mult_ x y) = do 
      Tint <- go vs x
      Tint <- go vs y 
      return Tint
    go vs (Equal_ x y) = do 
      xt <- go vs x
      xy <- go vs y 
      guard (xt == xy)
      return Tbool
    go vs (Less_ x y) = do 
      Tint <- go vs x
      Tint <- go vs y 
      return Tbool
    go vs (If_ c t e) = do 
      Tbool <- go vs c 
      tt <- go vs t 
      et <- go vs t 
      guard (tt == et) 
      return tt
    go vs (Let_ v b) = do 
      vt <- go vs v 
      go (vt : vs) b


typecheck :: L -> Maybe Type  
typecheck = 
    go []
  where 
    go _ (Iconst _) = Just Tint
    go _ (Bconst _) = Just Tbool
    go vs (Var v) = lookup v vs
    go vs (Plus x y) = do 
      Tint <- go vs x
      Tint <- go vs y 
      return Tint
    go vs (Mult x y) = do 
      Tint <- go vs x
      Tint <- go vs y 
      return Tint
    go vs (Equal x y) = do 
      xt <- go vs x
      xy <- go vs y 
      guard (xt == xy)
      return Tbool
    go vs (Less x y) = do 
      Tint <- go vs x
      Tint <- go vs y 
      return Tbool
    go vs (If c t e) = do 
      Tbool <- go vs c 
      tt <- go vs t 
      et <- go vs t 
      guard (tt == et) 
      return tt
    go vs (Let x v b) = do 
      vt <- go vs v 
      go ((x,vt) : vs) b

test' :: Ldb -> IO () 
test' s = do 
    putStrLn "\n================================="
    print s 
    putStrLn "---------------------------------"
    print $ typecheck_ [] s 
    putStrLn "---------------------------------"
    let db = toNormal "xyzabcdefghijklmnopqrstuvw" s 
    print db
    putStrLn "================================="
  where 
    maybePrint :: Show a => Maybe a -> IO () 
    maybePrint Nothing = putStrLn "Nothing"
    maybePrint (Just x) = print x 

test'' :: Ldb -> IO () 
test'' s = do 
    putStrLn "\n================================="
    print s 
    putStrLn "---------------------------------"
    print $ typecheck_ [Tint, Tbool] s 
    putStrLn "---------------------------------"
    let db = toNormal "xyzabcdefghijklmnopqrstuvw" s 
    print db
    putStrLn "================================="
  where 
    maybePrint :: Show a => Maybe a -> IO () 
    maybePrint Nothing = putStrLn "Nothing"
    maybePrint (Just x) = print x 


test :: L -> IO () 
test s = do 
    putStrLn "\n================================="
    print s 
    putStrLn "---------------------------------"
    let db = toDeBruijn s 
    maybePrint db
    putStrLn "---------------------------------"
    maybePrint $ toNormal "xyzabcdefghijklmnopqrstuvw" <$> db 
    putStrLn "---------------------------------"
    maybePrint $ typecheck s
    putStrLn "---------------------------------"
    maybePrint $ (db >>= interp) 
    putStrLn "================================="
  where 
    maybePrint :: Show a => Maybe a -> IO () 
    maybePrint Nothing = putStrLn "Nothing"
    maybePrint (Just x) = print x 


t :: L 
t = Let "x" (Iconst 13) (If (Equal (Var "x") (Iconst 42)) (Iconst 666) (Plus (Var "x") (Iconst 13))) 

main :: IO () 
main = do 
  test (Iconst 13) 
  test (Bconst True) 
  test (Var "x")  
  test (Plus (Iconst 13) (Iconst 42))
  test (Mult (Iconst 13) (Iconst 42))
  test (Equal (Iconst 13) (Iconst 42))
  test (Let "x" (Iconst 42) (Let "y" (Bconst False) (Plus (Var "x") (Var "y"))))
  test t 
  test $ Let "x" (If (Bconst False) (Mult (Iconst 13) (Iconst 1)) (Plus (Iconst 42) (Iconst 1))) (Let "y" (Let "x" (Bconst True) (If (Equal (Var "x") (Bconst True)) (Bconst False) (Var "x"))) (Less (Var "y") (Iconst 666)))  
  test $ Let "x" (If (Bconst False) (Mult (Iconst 13) (Iconst 1)) (Plus (Iconst 42) (Iconst 1))) (Let "y" (Let "x" (Bconst True) (If (Equal (Var "x") (Bconst True)) (Bconst False) (Var "x"))) (Less (If (Var "y") (Iconst 777) (Iconst 999)) (Iconst 666)))  
  test $ Let "x" (Let "x" (Iconst 13) (Var "x")) (Mult (Var "x") (Iconst 42))
  test $ Let "x" (Iconst 13) (Let "y" (Iconst 42) (Let "z" (Plus (Var "x") (Var "y")) (If (Less (Var "z") (Iconst 100)) (Equal (Var "z") (Var "x")) (Bconst False))))
  test $ Let "x" (Iconst 13) (Var "y")
  test' $ Let_ (Iconst_ 13) (Var_ 1)
  test'' $ Let_ (Mult_ (Var_ 0) (Var_ 0)) (Var_ 1) 











