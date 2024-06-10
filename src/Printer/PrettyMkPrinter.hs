module Printer.PrettyMkPrinter where

import           Def
import           Syntax
import           Program
import           Data.List          (intercalate)
import           Prettyprinter
import           Text.Printf        (printf)
import           Util.Miscellaneous (parenthesize)


prettyMk :: Program G X -> String 
prettyMk prg@(Program defs goal) = unlines (map prettyMkDef defs) ++ "\n" ++ "? " ++ prettyMkGoal goal

prettyMkDef :: Def G X -> String 
prettyMkDef def@(Def name args body) = 
  printf "%s %s = %s;\n" name (unwords args) (prettyMkGoal body)

prettyMkGoal :: G X -> String 
prettyMkGoal (t1 :=: t2) = printf "%s == %s" (prettyMkTerm t1) (prettyMkTerm t2)
prettyMkGoal (Conjunction x y gs) = printf "(%s)" (intercalate " & " $ prettyMkGoal <$> (x : y : gs))
prettyMkGoal (Disjunction x y gs) = printf "(%s)" (intercalate " | " $ prettyMkGoal <$> (x : y : gs))
prettyMkGoal (Fresh name g) =
  let (names, goal) = freshVars [name] g in
  printf "(fresh %s in (%s))" (intercalate ", " names) (prettyMkGoal goal)
prettyMkGoal (Invoke name ts) | null ts =
  printf "%s []" name
prettyMkGoal (Invoke name ts) =
  printf "%s %s" name (unwords $ map (parenthesize . prettyMkTerm) ts)
prettyMkGoal (Delay g) = printf "Delay (%s)" (prettyMkGoal g)

prettyMkTerm :: Term X -> String 
prettyMkTerm (V v) = v
prettyMkTerm (C name []) | isNil name = "[]"
prettyMkTerm (C name [h, C name1 []]) | isCons name && isNil name1 = printf "[%s]" (prettyMkTerm h)
prettyMkTerm (C name [h, t]) | isCons name = printf "(%s :: %s)" (prettyMkTerm h) (prettyMkTerm t)
--prettyMkTerm (C name [x, y]) | isPair name = printf "(%s, %s)" (prettyMkTerm x) (prettyMkTerm y)
prettyMkTerm (C name ts) =
  case ts of
    [] -> name
    _  -> printf "%s %s" name (unwords $ map (show . parens . pretty . prettyMkTerm) ts)