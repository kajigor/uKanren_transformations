module Residualization (transform) where
import Data
import MiniKanren
import Driver
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Data.List (nub, (\\))

type Ancestors = Integer -> Tree

addAnc :: Ancestors -> Integer -> Tree -> Ancestors
addAnc ancs i t  = \x -> if x == i then t else ancs x

transform :: [Def] -> String -> (String, [Def])
transform defs name =
  let x@(Def _ formalArgs _) = env' defs name
      goal = fresh formalArgs (Invoke name (map Var formalArgs))
  in  residualizeTopLevel $ drive (Spec defs goal)

topLevelName :: String
topLevelName = "topLevel"

residualizeTopLevel :: Tree -> (String, [Def])
residualizeTopLevel t =
  let spec = residualize' t
      collectFV (Fresh v g) acc = collectFV g (v:acc)
      collectFV g acc = (g, reverse acc)
      (body, formalArgs) = collectFV (goal spec) []
      topLevelDef = Def { name = topLevelName
                        , args = formalArgs
                        , body = body
                        }
  in  (topLevelName, topLevelDef : defs spec)

makeVar i = "x." ++ show i

newFname :: String -> [String] -> (String, [String])
newFname oldName fNames =
  if   oldName `elem` fNames
  then newFname (oldName ++ "'") fNames
  else (oldName, fNames)

getArgs :: Goal -> [Integer]
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

formalArgs :: Goal -> Goal -> [Term]
formalArgs goal call =
  let Just ren = renaming goal call
  in  map (\x -> freeToVar $ fromJust $ lookup (Free x) ren) (getArgs goal)

getName :: Goal -> String
getName (Invoke name _) = name
getName (Conj l r) = getName l ++ "_" ++ getName r
getName _ = "pred"

generateInvocation :: Goal -> [String] -> [(Goal,(String,Tree))] -> Goal -> Tree -> (String, [Term], [String], [(Goal,(String,Tree))])
generateInvocation goal fNames defs call anc =
  case lookup goal defs of
    Just (name,anc) -> (name, formalArgs goal call, fNames, defs)
    Nothing ->
      let name = getName goal
          (name', fn) = newFname name fNames
          def = (goal,(name',anc))
      in  (name', formalArgs goal call, fn, def:defs)

instantiate :: [(Term, Term)] -> Goal -> Goal
instantiate subst goal =
  let instantiate' = instantiate subst
  in  case goal of
        Unify l r -> Unify (instantiateT subst l) (instantiateT subst r)
        Conj l r -> Conj (instantiate' l) (instantiate' r)
        Disj l r -> Disj (instantiate' l) (instantiate' r)
        Zzz g -> Zzz (instantiate' g)
        Invoke name args -> Invoke name (map (instantiateT subst) args)
        Fresh v g -> Fresh v (instantiate' g)
  where
    instantiateT subst g =
      case g of
        Ctor name args -> Ctor name $ map (\x -> fromMaybe x (lookup x subst)) args
        x -> fromMaybe x (lookup x subst)

residualizeState :: State -> [Integer] -> [Goal]
residualizeState state bound =
  mapMaybe (\(v,u) -> if v `elem` bound then Just (Var (makeVar v) === freeToVar u) else Nothing )
           (reverse $ getSubst state)

freeToVar :: Term -> Term
freeToVar (Free x) = Var $ makeVar x
freeToVar (Ctor name args) = Ctor name (map freeToVar args)
freeToVar x = x

residualize' :: Tree -> Spec
residualize' t =
  let initAnc i = error $ "No ancestor with key " ++ show i
      (x,_,ds) = residualize t [] initAnc [topLevelName] []
      createDef (goal, (name, tree)) =
        let args = getArgs goal
            body = (\(x,_,_) -> x) $ residualize tree args initAnc [topLevelName] []
        in  Def name (map makeVar args) body
  in  Spec { goal = x, defs = map createDef ds }

residualize :: Tree -> [Integer] -> Ancestors -> [String] -> [(Goal,(String,Tree))] -> (Goal, [String], [(Goal,(String,Tree))])
residualize x bound ancs fNames defs =
  case x of
    Step i _ _ fv ch ->
      let (g, fn, td) = residualize ch (fv++bound) ancs fNames defs
      in  (fresh (map makeVar fv) g, fn, td)
    Or i _ _ ch ->
      let ancs' = addAnc ancs i x
          (gs, fn, td) =
            foldl (\(gs,fn,td) y ->
                      let (g, fn', td') = residualize y bound ancs' fn td
                      in  (g:gs, fn', td'))
                  ([], fNames, defs)
                  ch
      in  (disj (reverse gs), fn, td)
    Split i _ _ _ ch1 ch2 ->
      let ancs' = addAnc ancs i x
          (g, fn, td) = residualize ch1 bound ancs' fNames defs
          bound' = bound \\ map fst (getSubst $ state ch1)
          (g', fn', td') = residualize ch2 bound' ancs' fn td
      in  (g &&& g', fn', td')
    Success st -> (conj $ residualizeState st bound, fNames, defs)
    Fail -> error "Unexpected Fail node in the driving tree"
    x@(Renaming i st g) ->
      let anc = ancs i
          (name, args, fn, defs') = generateInvocation (getGoal anc) fNames defs g anc
          invoke = Invoke name args
      in  (conj $ residualizeState st bound ++ [invoke], fn, defs')
    Gen _ _ subst _ ch ->
      let (goal, fn, td) = residualize ch bound ancs fNames defs
          subst' = map (\(x,y) -> (freeToVar x, freeToVar y)) subst
      in  (instantiate subst' goal, fn, td)
