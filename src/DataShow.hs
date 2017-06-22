module DataShow where
import Data
import Data.List (intercalate)

showSubst f s = "[" ++ intercalate ", " (map (\(i,t) -> f i ++ " -> " ++ show t) s) ++  "]"

instance Show State where
  show (State subst state index vars) = "St " ++ showSubst (\i -> "x." ++ show i) subst ++
--                                        "\n" ++ showSubst id (map (\x -> (x, state x)) vars) ++
                                        " " ++ show index

instance Show Term where
  show (Var v) = v
  show (Free i) = "x." ++ show i
  show (Ctor ctor ts) = ctor ++ "(" ++ showList ts ")"

instance Show Goal where
  show (Unify l r) = "(" ++ show l ++ ") === (" ++ show r ++ ")"
  show (Conj l r)  = "(" ++ show l ++ ") &&& (" ++ show r ++ ")"
  show (Disj l r)  = "(" ++ show l ++ ") ||| (" ++ show r ++ ")"
  show (Fresh v g) = "Fresh " ++ v ++ " " ++ show g
  show (Invoke n args) = "Invoke " ++ n ++ " with (" ++ show args ++ ")"
  show (Zzz g) = "Zzz " ++ show g

instance Show a => Show (Stream a) where
  show Empty = "[]"
  show (Mature a s) = show a ++ " :\n" ++ show s
  show (Immature s) = "Immature " ++ show s

instance Show Tree where
  show t =
    show' t 0
    where
      nSpaces n = replicate n ' '
      show' t n =
        case t of
          Fail                        -> nSpaces n ++ "F"
          Success st                  -> nSpaces n ++ "S " ++ show st
          Renaming i st g             -> nSpaces n ++ "R " ++ show i ++ " " ++ show st ++ " (" ++ show g ++ ")"
          Step     i st g ch          -> nSpaces n ++ "T " ++ show i ++ " " ++ show st ++ " (" ++ show g ++ ")" ++ "\n" ++ show' ch (n+1)
          Or       i st g ch          -> nSpaces n ++ "O " ++ show i ++ " " ++ show st ++ " (" ++ show g ++ ")" ++ "\n" ++ intercalate "\n" (map (\x -> show' x (n+1)) ch)
          Split    i st g1 g2 ch1 ch2 -> nSpaces n ++ "G " ++ show i ++ " " ++ show st ++ " (" ++ show g1 ++ ")" ++ " (" ++ show g2 ++ ")" ++ "\n" ++ show' ch1 (n+1) ++ "\n" ++ show' ch2 (n+1)
          Gen      i st g ch          -> nSpaces n ++ "A " ++ show i ++ " " ++ show st ++ " (" ++ show g ++ ")" ++ "\n" ++ show' ch (n+1)






