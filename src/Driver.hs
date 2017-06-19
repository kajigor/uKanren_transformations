module Driver where
import Data
import Data.Foldable (foldrM, msum)
import Data.List (find, intercalate)
import Control.Monad (mplus)
import Data.Maybe (mapMaybe, isJust, fromMaybe)
import MiniKanren
import Debug.Trace
import State

updateRenaming :: Term -> Term -> Renaming -> Maybe Renaming
updateRenaming l r renaming =
  if l `elem` fst (unzip renaming)
  then if (l,r) `elem` renaming
       then Just renaming
       else Nothing
  else Just ((l,r):renaming)

renaming :: Goal -> Goal -> Bool
renaming l r =
  isJust $ rename l r []
  where
    rename l r renaming =
      case (l,r) of
        (Unify l l', Unify r r') -> renameT l r renaming >>= renameT l' r'
        (Conj  l l', Conj  r r') -> rename  l r renaming >>= rename l' r'
        (Disj  l l', Disj  r r') -> rename  l r renaming >>= rename l' r'
        (Zzz   l,    Zzz   r)    -> rename  l r renaming
        (Invoke ln ls, Invoke rn rs) | ln == rn ->
          foldrM (\(t,t') r -> renameT t t' r) renaming (zip ls rs)
        _ -> Nothing
    renameT l r renaming =
      case (l,r) of
        (Free _, Free _) -> updateRenaming l r renaming
        (Var  _, Free _) -> updateRenaming l r renaming
        (Var  _, Var  _) -> updateRenaming l r renaming
        (Free _, Var  _) -> updateRenaming l r renaming
        (Ctor ln ls, Ctor rn rs) | ln == rn ->
          foldrM (\(t,t') r -> renameT t t' r) renaming (zip ls rs)
        _ -> Nothing

class Embeddable a where
  couple :: a -> a -> Renaming -> Maybe Renaming
  dive   :: a -> a -> Renaming -> Maybe Renaming
  embed  :: a -> a -> Renaming -> Maybe Renaming
  embed l r renaming = couple l r renaming `mplus` dive l r renaming

instance Embeddable Term where
  couple l r renaming =
    case (l,r) of
      (Free _, Free _) -> updateRenaming l r renaming
      (Var  _, Var  _) -> updateRenaming l r renaming
      (Ctor ln ls, Ctor rn rs) | ln == rn ->
        foldrM (\(t,t') r -> embed t t' r) renaming (zip ls rs)
      _ -> Nothing

  dive l r renaming =
    case r of
      Ctor _ rs -> msum (map (\r' -> embed l r' renaming) rs)
      _ -> Nothing

instance Embeddable Goal where
  couple l r renaming =
--    trace ("\nChecking for coupling\n" ++ show l ++ "\n" ++ show r ++ "\n") $
    case (l,r) of
      (Unify l l', Unify r r') -> embed l r renaming >>= embed l' r'
      (Conj  l l', Conj  r r') -> embed l r renaming >>= embed l' r'
      (Disj  l l', Disj  r r') -> embed l r renaming >>= embed l' r'
      (Zzz   l,    Zzz   r)    -> embed l r renaming
      (Invoke ln ls, Invoke rn rs) | ln == rn ->
        foldrM (\(t,t') r -> embed t t' r) renaming (zip ls rs)
      _ -> Nothing

  dive l r renaming =
    case r of
      Zzz  r    -> embed l r renaming
      Conj r r' -> embed l r renaming `mplus` embed l r' renaming
      Disj r r' -> embed l r renaming `mplus` embed l r' renaming
      _ -> Nothing

mergeCtx :: Goal -> Ctx -> Goal
mergeCtx = foldl Conj

unfold :: Goal -> State -> [([Goal], State)]
unfold goal state =
  -- trace ("in unfold " ++ show goal ++ "\nstate: " ++ show state ) $
  case goal of
    Unify l r ->
      case unify state l r of
        Nothing -> []
        Just st -> [([], st)]
    Disj l r -> unfold l state ++ unfold r state
    Zzz g -> unfold g state
    Conj l r ->
      let unfL = unfold l state
      in  concatMap
            (\(g,st) ->
                case g of
                  [] -> unfold (substG state r) st
                  [g''] ->
                    map (\(g',st') ->
                            case g' of
                              [] -> (g, st')
                              [g'] -> ([Conj g'' g'], st))
                        (unfold (substG state r) st))
            (reverse unfL)
    Invoke _ _ -> [([substG state goal], state)]
    Fresh var g -> unfold g st
                   where st = newVar state var

--applyState goal state =
--  case goal of
--    Unify l r -> Unify (apply l state) (apply r state)
--    Disj  l r -> Disj  (applyState l state) (applyState r state)
--    Conj  l r -> Conj  (applyState l state) (applyState r state)
--    Fresh v g -> Fresh v (applyState g state)
--    Zzz     g -> Zzz (applyState g state)
--    Invoke n args -> Invoke n (map (`apply` state) args)
--  where
--    apply v state =
--      case v of
--        Free i -> fromMaybe (Free i) (lookup i (getSubst state))
--        Var  v -> if v `elem` vars state then getState state v else Var v
--        Ctor n ts -> Ctor n (map (`apply` state) ts)

goalToList (Conj l r) = goalToList l ++ goalToList r
goalToList x = [x]

split (x:xs) (y:ys) | isJust $ couple x y [] =
  let (cs,uncs) = split xs ys in (x:cs, uncs)
split xs [] = ([], xs)

--split :: [Goal] -> [Goal] -> Tree -- ([Goal], [Goal])
--split xs ys =
--  let (bef,lat) = span (\(x,y) -> isJust $ couple x y []) (zip xs ys)
--  in  trace ("\nSPLIT\n" ++ show bef ++ "\n" ++ show lat ++ "\n") $ undefined

drive :: Spec -> Tree
drive spec =
--  drive' :: Integer -> Goal -> Ctx -> State -> [(Integer,Goal)] -> Tree
  drive' 0 (goal spec) [] emptyState []
  where
    drive' i g' ctx state ancs =
      let anc = substG state (mergeCtx g' ctx) -- trace (show g) $ substG state (mergeCtx g ctx)
          ancs' = (i,anc) : ancs
          g = substG state g'
      in  --if i >= 13 then Fail else
          case g of
            Unify l r ->
              case unify state l r of
                Just st ->
                  case ctx of
                    [] -> Success st
                    (c:cs) -> drive' (i+1) c cs st ancs'
                Nothing -> Fail
            Zzz g -> drive' i g ctx state ancs'
            Disj l r -> Or i state anc (drive' (i+1) l ctx state ancs')
                                       (drive' (i+1) r ctx state ancs')
            Conj l r -> drive' (i+1) l (substG state r:ctx) state ancs'
            Fresh var g -> Step i state anc (drive' (i+1) g ctx st ancs')
                           where st = newVar state var
            Invoke name actualArgs ->
              let (Def _ formalArgs body) = env spec name
                  state' = foldl bindVar state $ zip formalArgs (map (subst state) actualArgs)
                  unf = unfold body state'
                  processCtx st acc [] = Just (st,acc)
                  processCtx st acc (c:cs) =
                    case unfold c st of
                      [] -> Nothing
                      [(g,s)] -> processCtx s (acc ++ g) cs
                      _ -> processCtx st (c:acc) cs
                  res =
--                    trace ("\nUNFOLDED " ++ show unf ++ "\n") $
                    mapMaybe (\(g,st) -> processCtx st g ctx) unf
                  makeNode st goals =
                    case goals of
                      [] -> Success st
                      _ ->
                        let conjunction = conj goals
                        in
--                          trace ("\nAncestors! " ++ (intercalate "\n" (map show ancs')) ++ "\n" ) $
--                          trace ("\nCONJUNCTION " ++ show conjunction ++ "\n") $
                          case find (\(n,g) -> renaming g conjunction) ancs' of
                             Just (n,g) -> Renaming n st g
                             Nothing ->
                               case --trace ("\nCOUPLE TEST\n" ++ (intercalate "\n" (map show ancs')) ++ "\n" ) $
                                    find (\(n,g) -> isJust $ couple g conjunction []) ancs' of
                                 Just (n,g) ->
                                   let current = goalToList conjunction
                                       prev = goalToList g
                                       cut = split current prev
                                   in
                                     if length current > length prev
                                     then
                                       let (l,r) = split current prev
                                           l' = case find (\(n,g) -> renaming g (conj l)) ancs' of
                                                  Just (n,g) -> Renaming n st g
                                                  Nothing -> drive' (i+1) (conj l) [] st ancs'
                                           r' = case find (\(n,g) -> renaming g (conj r)) ancs' of
                                                  Just (n,g) -> Renaming n st g
                                                  Nothing -> drive' (i+1) (conj r) [] st ancs'
                                       in Or i state' anc l' r'
                                     else error "Coupled conjunctions have different number of conjuncts."
                                 Nothing -> drive' (i+1) conjunction [] st ancs'
                  ch =
                    let nodes = map (uncurry makeNode) res
                    in  if length nodes > 1
                        then foldl1 (Or (i + 1) state' anc) nodes
                        else if null nodes then Fail else head nodes
              in Step i state' anc ch











