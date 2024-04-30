
a0 = 
a1 = (True, False)
a2 = (False, True) 
a3 = (True, True)
genP :: MonadPlus m => m (Bool, Bool)
genP = (return a1) <|> (return a2) <|> (return a3)

evalP :: MonadPlus m => Bool -> m Term
evalP True = do 
  x <- evalP True 
  y <- evalP True 
  return (Conj x y) 
evalP False = do 
  (u, v) <- genP 
  x <- evalP u 
  y <- evalP v 
  return (Conj x y)

eval :: [(String, Bool)] -> Expr -> Bool
eval st fm@(Disj x y) = 
    let u = eval st x in 
    let v = eval st y in 
    or u v 


gen st res | res == True = 
    xt <- gen st True 
    xf <- gen st False

    return 

gen st res | res == False = 
    x <- gen False 
    y <- gen False 
    return Disj x y 

