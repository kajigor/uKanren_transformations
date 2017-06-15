import Debug.Trace
import Data.List
import Control.Monad
import Control.Applicative
import Data.Maybe 

-- Abstraction synonyms
type Name = String
type Var  = Int

-- Terms
data Term = C Name [Term] | L Var | V Name deriving (Eq, Show)

-- Goals
data G = 
    Term :=: Term
  | G :&: G
  | G :!: G
  | Fresh  Name G 
  | Invoke Name [Term] deriving Show

infixr 7 &&&
infixr 6 |||
infix  8 ===

(===) = (:=:)
(|||) = (:!:)
(&&&) = (:&:)

fresh xs g = foldr Fresh g xs
call       = Invoke

-- Definitions
type D = (Name, [Name], G)

def = (,,)

-- Specification
type S = ([D], G)

spec = (,)

-- Interpreter
type State = (String -> Term, [(Var, Term)], Int)

update bnds x v y = if y == x then v else bnds y

subst  st         (C n ts) = C n $ map (subst st) ts
subst  (st, _, _) (V n)    = st n
subst  _          t        = t

substG st                   (t1 :=: t2)   = (subst  st t1) :=: (subst  st t2)
substG st                   (g1 :!: g2)   = (substG st g1) :!: (substG st g2)
substG st                   (g1 :&: g2)   = (substG st g1) :&: (substG st g2)
substG (bnds, subst, index) (Fresh x g)   = Fresh x (substG (update bnds x (V x), subst, index) g)
substG st                   (Invoke f as) = Invoke f (map (subst st) as)

eval env state (t1 :=: t2)   = maybeToStream $ unify (Just state) (subst state t1) (subst state t2)
eval env state (g1 :!: g2)   = eval env state g1 `mplus` eval env state g2
eval env state (g1 :&: g2)   = eval env state g1 >>= (\ st -> eval env st (substG state g2))
eval env state (Fresh x g)   = eval env (freshVar state x) g
eval env state (Invoke f as) = 
  let (_, fs, g) = env f in
  let state'     = foldl bindVar state $ zip fs as in
  eval env state' g

bindVar  st@(bnds, sub, index) (fa, aa) = (update bnds fa (subst st aa), sub, index  )
freshVar st@(bnds, sub, index) x        = (update bnds x  (L index)    , sub, index+1)

unify Nothing _ _ = Nothing
unify st@(Just (s@(bnds, subst, index))) u v = 
  unify' (walk u subst) (walk v subst)  where
    unify' (L u) (L v) | u == v = Just s
    unify' (L u) _              = Just (bnds, extS u v subst, index)
    unify' _ (L v)              = Just (bnds, extS v u subst, index)
    unify' (C a as) (C b bs) | a == b && length as == length bs = foldl (\ st (u, v) -> unify st u v) st $ zip as bs
    unify' (V _) _ = error "syntactic variable in unification"
    unify' _ (V _) = error "syntactic variable in unification"
    unify' _ _ = Nothing
    walk x@(L v) s =
      case lookup v s of
        Nothing -> x
        Just t  -> walk t s
    walk (V _) _ = error "syntactic variable in walk"
    walk u     _ = u
    extS u v s = (u, v) : s

data Stream a = Empty
              | Mature a (Stream a)
              -- we need this in case of left recursion (who would have known)
              | Immature (Stream a)
              deriving Show

takeS 0 _            = []
takeS n Empty        = []
takeS n (Mature a s) = a : takeS (n-1) s
takeS n (Immature s) = takeS n s

maybeToStream Nothing  = Empty
maybeToStream (Just a) = return a

instance Functor Stream where
  fmap _ Empty        = Empty
  fmap f (Mature a s) = Mature (f a) (fmap f s)
  fmap f (Immature s) = Immature (fmap f s)

instance Applicative Stream where
  pure a = Mature a Empty
  Empty        <*> _            = Empty
  (Mature f s) <*> Empty        = Empty
  (Immature s) <*> x            = s <*> x
  (Mature f s) <*> (Mature x t) = Mature (f x) (s <*> t)
  s            <*> (Immature t) = s <*> t

instance Alternative Stream where
  empty = Empty
  Empty <|> s = s
  s <|> Empty = s
  (Immature s) <|> t = s <|> t
  s <|> (Immature t) = s <|> t
  (Mature a s) <|> t = Mature a (s <|> t)

instance Monad Stream where
  Empty >>= _ = mzero
  (Mature x xs) >>= g = g x `mplus` (xs >>= g)
  (Immature _1) >>= _2 = Immature (_1 >>= _2)

instance MonadPlus Stream where
  mzero = Empty
  mplus (Mature h tl) _2 = Mature h (_2 `mplus` tl)
  mplus (Immature _1) _2 = Immature (_2 `mplus` _1)
  mplus Empty         _2 = _2

state0 = ((\ _ -> undefined), [], 0)

run :: S -> Stream State
run (defs, goal) =
  let defs' = map (\ d@(n, _, _) -> (n, d)) defs in
  eval (\ n -> fromJust $ lookup n defs') state0 goal

-- Tests
nil      = C "Nil"  []     
cons x y = C "Cons" [x, y] 
i    x   = C x      []

appendo =
  let x  = V "x"  in
  let y  = V "y"  in
  let xy = V "xy" in
  let h  = V "h"  in
  let t  = V "t"  in
  let ty = V "ty" in
  def "appendo" ["x", "y", "xy"] 
         ((x === nil &&& xy === y) ||| 
          (fresh ["h", "t", "ty"] 
             (x  === h `cons` t  &&&
              xy === h `cons` ty &&&
              call "appendo" [t, y, ty]
             )
          )
         )

reverso =
  let x  = V "x"  in
  let y  = V "y"  in
  let h  = V "h"  in
  let t  = V "t"  in
  let rt = V "rt" in
  def "reverso" ["x", "y"]
         ((x === nil &&& y === nil) |||
          (fresh ["h", "t", "rt"]
             (x === h `cons` t &&&
              call "appendo" [rt, h `cons` nil, y] &&&
              call "reverso" [t, rt] 
             )
          )
         )

toplevel n spec = takeS n $ fmap (\ (_, s, _) -> s) (run spec)

main = 
  do
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [i "A" `cons` nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [i "A" `cons` nil, i "B" `cons` nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [ V "q", i "B" `cons` nil, i "A" `cons` (i "B" `cons` nil)])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [i "A" `cons` nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [i "A" `cons` (i "B" `cons` nil), V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [V "q", i "A" `cons` (i "B" `cons` nil)])))
    

    
  
