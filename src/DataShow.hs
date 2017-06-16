module DataShow where
import Data
import Data.List (intercalate)

showSubst f s = "[" ++ intercalate ", " (map (\(i,t) -> f i ++ " -> " ++ show t) s) ++  "]"

instance Show State where
  show (State subst state index vars) = "St " ++ showSubst (\i -> "x." ++ show i) subst ++ "\n" ++
                                        showSubst id (map (\x -> (x, state x)) vars) ++
                                        " " ++ show index

instance Show Term where
  show (Var v) = v
  show (Free i) = "x." ++ show i
  show (Ctor ctor ts) = ctor ++ "(" ++ showList ts ")"

instance Show Goal where
  show (Unify l r) = "(" ++ show l ++ " === " ++ show r ++ ")"
  show (Conj l r)  = "(" ++ show l ++ " &&& " ++ show r ++ ")"
  show (Disj l r)  = "(" ++ show l ++ " ||| " ++ show r ++ ")"
  show (Fresh v g) = "Fresh " ++ v ++ " " ++ show g
  show (Invoke n args) = "Invoke " ++ n ++ " with (" ++ show args ++ ")"
  show (Zzz g) = "Zzz " ++ show g

instance Show a => Show (Stream a) where
  show Empty = "[]"
  show (Mature a s) = show a ++ " :\n" ++ show s
  show (Immature s) = "Immature " ++ show s
