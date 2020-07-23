module Util.ToProlog where

import Syntax
import Purification

import Data.Char
import Data.List

import Text.ParserCombinators.Parsec

import Text.Printf


{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}

unifyTerms :: Term X -> Term X -> Subst -> Maybe Subst
unifyTerms x y s = unify' (walk x s) (walk y s) where
  unify' (V v1)   (V v2)   | v1 == v2                         = Just s
  unify' (V v)    t                                           = occursCheck v t $ (v, t) : s
  unify' t        (V v)                                       = occursCheck v t $ (v, t) : s
  unify' (C a as) (C b bs) | a == b && length as == length bs = foldl (\st (t1, t2) -> st >>= unifyTerms t1 t2) (Just s) $ zip as bs
  unify' _        _                                           = Nothing

  walk t@(V v) s = case lookup v s of
                   Nothing -> t
                   Just t' -> walk t' s
  walk t       _ = t

  occursCheck v t s = if elem v $ fv t then Nothing else Just s

{-------------------------------------------}
goalToDNF :: G X -> [(Subst, Funcs)]
goalToDNF g = toDNF [] g where
  toDNF s (t1 :=: t2)  = case unifyTerms t1 t2 s of
                         Nothing -> []
                         Just s' -> [(s', [])]
  toDNF s (Invoke n a) = [(s, [(n, a)])]
  toDNF s (Fresh _ g') = toDNF s g'
  toDNF s (g1 :\/: g2) = toDNF s g1 ++ toDNF s g2
  toDNF s (g1 :/\: g2) = [(s2, f1 ++ f2) | (s1, f1) <- toDNF s g1, (s2, f2) <- toDNF s1 g2]

{-------------------------------------------}
applySigma :: Subst -> Term X -> Term X
applySigma s t@(V v) = case lookup v s of
                       Nothing -> t
                       Just t' -> applySigma s t'
applySigma s (C n a) = C n $ map (applySigma s) a

{-------------------------------------------}
applyInFunc :: Subst -> Func -> Func
applyInFunc s (n, a) = (n, map (applySigma s) a)


{-------------------------------------------}
defToProlog :: Def -> Rules
defToProlog (Def n a g) = map (\(s, f) -> (applyInFunc s (n, map V a), map (applyInFunc s) f)) $ goalToDNF g

{-------------------------------------------}
goalToProlog :: G X -> [Funcs]
goalToProlog g = map (\(s, f) -> map (applyInFunc s) f) $ goalToDNF g

defsToProlog :: [Def] -> String
defsToProlog defs =
  let rules = concatMap defToProlog defs in
  printRules rules

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}

replaceChars '\'' = "_0"
replaceChars c   = [c]

toConstrName :: String -> String
toConstrName "%"   = "cons"
toConstrName (c:s) = concatMap replaceChars $ toLower c : s

{-------------------------------------------}
toVarName :: String -> String
toVarName (c:s) = concatMap replaceChars $ toUpper c : s

{-------------------------------------------}
toFuncName :: String -> String
toFuncName = toConstrName

{-------------------------------------------}
printTerm :: Term X -> String
printTerm (V x)    = toVarName x
printTerm (C n []) = toConstrName n
printTerm (C n a)  = printf "%s(%s)" (toConstrName n) (intercalate ", " (map printTerm a))

{-------------------------------------------}
printFunc :: Func -> String
printFunc (n, a) = printf "%s(%s)" (toFuncName n) (intercalate ", " (map printTerm a))

{-------------------------------------------}
printFuncs :: Funcs -> String
printFuncs = intercalate ", " . map printFunc

{-------------------------------------------}
printRule :: Rule -> String
printRule (h, []) = printf "%s." (printFunc h)
printRule (h, t) = printf "%s :- %s." (printFunc h) (printFuncs t)

{-------------------------------------------}
printRules :: Rules -> String
printRules = intercalate "\n" . map printRule

{-------------------------------------------}
printGoal :: Funcs -> String
printGoal fs = printf ":- %s." (printFuncs fs)
-- printGoal = (++ ".") . (":- " ++) . printFuncs

{-------------------------------------------}
printGoals :: [Funcs] -> String
printGoals = intercalate "\n" . map printGoal

{-------------------------------------------}
printProg :: (Rules, [Funcs]) -> String
printProg ([], []) = ""
printProg (rs, []) = printRules rs
printProg ([], gs) = printGoals gs
printProg (rs, gs) = printf "%s\n%s" (printRules rs) (printGoals gs)

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}

commentP :: Parser ()
commentP =
  let rest = many (noneOf "*") >> char '*' >> (string "/" <|> rest) in
  string "/*" >> rest >> return ()

{-------------------------------------------}
voidP :: Parser ()
voidP = skipMany (commentP <|> skipMany1 space)

{-------------------------------------------}
nameP :: Parser String
nameP = do
  first <- lower
  rest  <- many (letter <|> digit <|> char '_')
  voidP
  return (first : rest)

{-------------------------------------------}
varP :: Parser (Term X)
varP = do
  first <- upper
  rest  <- many (letter <|> digit <|> char '_')
  voidP
  return (V $ first : rest)

{-------------------------------------------}
enumP :: Parser a -> Parser [a]
enumP argP = do
  first <- argP
  rest  <- many (char ',' >> voidP >> argP)
  return (first : rest)

{-------------------------------------------}
enumInBracketsP :: Parser a -> Parser [a]
enumInBracketsP argP = do
  char '('
  voidP
  enum <- enumP argP
  char ')'
  voidP
  return enum

{-------------------------------------------}
listEmptyRestP :: Parser (Term X)
listEmptyRestP = do
  char ']'
  voidP
  return (C "nil" [])

{-------------------------------------------}
listNonemptyLastP :: Parser (Term X)
listNonemptyLastP = do
  char '|'
  voidP
  rest <- termP
  char ']'
  voidP
  return rest

{-------------------------------------------}
listNonemptyRestP :: Parser (Term X)
listNonemptyRestP = do
  args <- enumP termP
  rest <- (listNonemptyLastP <|> listEmptyRestP)
  return (foldr (\x acc -> C "cons" [x, acc]) rest args)

{-------------------------------------------}
listP :: Parser (Term X)
listP = char '[' >> voidP >> (listEmptyRestP <|> listNonemptyRestP)


{-------------------------------------------}
constrP :: Parser (Term X)
constrP = do
  name <- nameP
  args <- option [] $ enumInBracketsP termP
  return (C name args)

{-------------------------------------------}
termP :: Parser (Term X)
termP = varP <|> constrP <|> listP

{-------------------------------------------}
funcP :: Parser Func
funcP = do
  name <- nameP
  args <- option [] $ enumInBracketsP termP
  return (name, args)

{-------------------------------------------}
bodyP :: Parser Funcs
bodyP = option [] $ string ":-" >> voidP >> enumP funcP

{-------------------------------------------}
ruleP :: Parser Rule
ruleP = do
  hd   <- funcP
  body <- bodyP
  char '.'
  voidP
  return (hd, body)

{-------------------------------------------}
rulesP :: Parser Rules
rulesP = many ruleP

{-------------------------------------------}
getRules :: String -> Rules
getRules s = case parse (voidP >> rulesP) "" s of
             Left  m -> error $ show m
             Right r -> r

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}

sepRules :: Rules -> [Rules]
sepRules []               = []
sepRules (x@((n,_),_):xs) = let (g,r) = partition ((n==) . fst . fst) xs in
                            (x:g) : sepRules r

{-------------------------------------------}
normalizeTerm :: Term X -> Term X
normalizeTerm (V v)        = V $ 'f' : v
normalizeTerm (C "cons" a) = C "%" $ map normalizeTerm a
normalizeTerm (C n a)      = C n   $ map normalizeTerm a

{-------------------------------------------}
normalizeFunc :: Func -> Func
normalizeFunc (n, a) = (n, map normalizeTerm a)

{-------------------------------------------}
normalizeRules :: Rules -> Rules
normalizeRules = map (\(h,b) -> (normalizeFunc h, map normalizeFunc b))

{-------------------------------------------}
ruleToG :: [X] -> Rule -> G X
ruleToG v ((name, a ), b ) =
  let conjs1 = map (\(v,t) -> V v === t) $ zip v a in
  let conjs2 = map (\(n,a) -> Invoke n a) b in
  let conjs  = conjs1 ++ conjs2 in
  let g      = if null conjs then success else foldr1 (&&&) conjs in
  fresh (fvg g \\ v) g

{-------------------------------------------}
rulesToDef :: Rules -> Def
rulesToDef rs@(((n,a),_):_) =
  let v     = map (('z':) . show) [1..length a] in
  let disjs = map (ruleToG v) rs in
  Def n v $ foldr1 (|||) disjs

{-------------------------------------------}
prologToG :: String -> (G X, [String], [Def])
prologToG pr =
  let rules@((((n,a),_):_):_) = sepRules $ normalizeRules $ getRules pr in
  let defs = map rulesToDef rules in
  let vars = map (('y':) . show) [1..length a] in
  let g    = Invoke n (map V vars) in
  (g, vars, defs)