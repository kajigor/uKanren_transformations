module Residualization where
import Data
import MiniKanren
import Driver
import Debug.Trace (trace)
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Data.List (intercalate, nub)
import State

addAnc :: Integer -> Tree -> (Integer -> Tree) -> (Integer -> Tree)
addAnc i t ancs = \x -> if x == i then t else ancs x

makeVar i = "x." ++ show i

newFname :: String -> [String] -> (String, [String])
newFname oldName fNames =
  if   oldName `elem` fNames
  then newFname (oldName ++ "'") fNames
  else (oldName, fNames)

getArgs goal =
  let getArgsT t =
        case t of
          Free k -> [k]
          Ctor _ ts -> concatMap getArgsT ts
          Var _ -> error "Unexpected syntactic variable"
      ga goal =
        case goal of
          Invoke _ args -> concatMap getArgsT args
          Conj l r -> ga l ++ ga r
          _ -> error $ "Unexpected goal" ++ show goal
  in  nub $ ga goal

formalArgs goal call =
  let args = getArgs goal
      Just ren = renaming goal call
  in  map (\x -> fromJust $ lookup (Free x) ren) args

getName (Invoke name _) = name
getName (Conj l r) = getName l ++ "_" ++ getName r
getName _ = "pred"

generateInvocation goal fNames defs call anc =
  case lookup goal defs of
    Just (name,anc) -> (name, formalArgs goal call, fNames, defs)
    Nothing ->
      let name = getName goal
          (name', fn) = newFname name fNames
      in  (name', formalArgs goal call, fn, (goal,(name',anc)):defs)

residualizeState state bound =
  mapMaybe (\(v,u) -> if v `elem` bound then Just (Free v === u) else Nothing )
           (reverse $ getSubst state)

residualize' t =
  let initAnc i = error $ "\nNo ancestor with key: " ++ show i
      (x,_,ds) = residualize t [] initAnc [] []
      createDef (goal, (name, tree)) =
        let args = getArgs goal
            body = (\(x,_,_) -> x) $ residualize tree args initAnc [] []
        in  Def name (map makeVar args) body
  in  Spec { goal = x, defs = map createDef ds }

residualize :: Tree -> [Integer] -> (Integer -> Tree) -> [String] -> [(Goal,(String,Tree))] -> (Goal, [String], [(Goal,(String,Tree))])
residualize x bound ancs fNames defs =
  case x of
    Step i _ _ fv ch ->
      let (g, fn, td) = residualize ch (fv++bound) ancs fNames defs
      in  (fresh (map makeVar fv) g, fn, td)
    Or i _ _ ch ->
      let ancs' = addAnc i x ancs
          (gs, fn, td) =
            foldl (\(gs,fn,td) y ->
                      let (g, fn', td') = residualize y bound ancs' fn td
                      in  (g:gs, fn', td'))
                  ([], fNames, defs)
                  ch
      in  (disj (reverse gs), fn, td)
    Split i _ _ _ ch1 ch2 ->
      let ancs' = addAnc i x ancs
          (g, fn, td) = residualize ch1 bound ancs' fNames defs
          (g', fn', td') = residualize ch2 bound ancs' fn td
      in  (g &&& g', fn', td')
    Success st ->
      ( conj $ residualizeState st bound
      , fNames
      , defs
      )
    Fail -> error "Unexpected Fail node in the driving tree"
    x@(Renaming i st g) ->
      let anc = ancs i
          (name, args, fn, defs') = generateInvocation (getGoal anc) fNames defs g anc
          invoke = Invoke name args
      in  (conj $ residualizeState st bound ++ [invoke], fn, defs')
    x@(Gen i st subst g ch) ->
      let y@(name, args, fn, defs') = generateInvocation (getGoal ch) fNames defs g ch
          actualArgs = map (\x -> fromMaybe x (lookup x subst)) args
          invoke = Invoke name actualArgs
      in  (conj $ residualizeState st bound ++ [invoke], fn, defs')
    -- x -> error $ "residualization of (" ++ show x ++ ") failed"
