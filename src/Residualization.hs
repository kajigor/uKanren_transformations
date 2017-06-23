module Residualization where
import Data
import MiniKanren
import Driver
import Debug.Trace (trace)
import Data.Maybe (mapMaybe, fromJust)
import Data.List (intercalate)

addAnc :: Integer -> Tree -> (Integer -> Tree) -> (Integer -> Tree)
addAnc i t ancs = --trace ("\nadding " ++ show i ++ " -> " ++ show t ++ "\n") $
  \x -> if x == i then t else ancs i

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
  in  case goal of
        Invoke _ args -> concatMap getArgsT args
        _ -> error $ "Unexpected goal" ++ show goal

formalArgs goal call =
  let args = getArgs goal
      Just ren = renaming goal call
  in  map (\x -> fromJust $ lookup (Free x) ren) args

generateInvocation goal fNames defs call anc =
  case lookup goal defs of
    Just (name,anc) -> (name, formalArgs goal call, fNames, defs)
    Nothing ->
      let (Invoke name args) = goal
          (name', fn) = newFname name fNames
      in  (name', formalArgs goal call, fn, (goal,(name',anc)):defs)

residualizeState state bound =
  mapMaybe (\(v,u) -> if v `elem` bound then Just (Free v === u) else Nothing ) (getSubst state)

residualize' t =
  let initAnc = (\i -> error $ "No ancestor with key: " ++ show i)
      (x,_,ds ) = residualize t [] initAnc [] []
      createDef (goal, (name, tree)) =
        let args = getArgs goal
            body = (\(x,_,_) -> x) $ residualize tree args initAnc [] []
        in  Def name (map makeVar $ args) body
  in  trace ("\nResidualized!\nDefs:\n" ++ intercalate "\n" (map show ds) ++ "\n") $
      Spec { goal = x, defs = map createDef ds }
residualize :: Tree -> [Integer] -> (Integer -> Tree) -> [String] -> [(Goal,(String,Tree))] -> (Goal, [String], [(Goal,(String,Tree))])
residualize x bound ancs fNames defs =
  case x of
    Step i _ _ fv ch ->
      let (g, fn, td) = residualize ch (fv++bound) ancs fNames defs
      in  (fresh (map makeVar fv) g, fn, td)
    Or i _ _ ch ->
      let (gs, fn, td) =
            foldl (\(gs,fn,td) y ->
                      let (g, fn', td') = residualize y bound (addAnc i x ancs) fn td
                      in  (g:gs, fn', td'))
                  ([], fNames, defs)
                  ch
      in  (disj gs, fn, td)
    Split i _ _ _ ch1 ch2 ->
      let (g, fn, td) = residualize ch1 bound (addAnc i x ancs) fNames defs
          (g', fn', td') = residualize ch2 bound (addAnc i x ancs) fn td
      in  (g &&& g', fn', td')
    Success st ->
      ( conj $ residualizeState st bound
      , fNames
      , defs
      )
    Fail -> error "Unexpected Fail node in the driving tree"
    Renaming i st g ->
      let anc = ancs i
          (name, args, fn, defs') = generateInvocation (getGoal anc) fNames defs g anc
          invoke = Invoke name args
      in  (conj $ (residualizeState st bound ++ [invoke]), fn, defs')
    x -> error $ "residualization of (" ++ show x ++ ") failed"
