
module InvokeAnnotation where


import           Data.Char          (toLower)
import           Data.List          (intercalate, nub)
import           Data.List.NonEmpty (NonEmpty (..), toList)
import           Text.Printf        (printf)
import           Util.Miscellaneous (parenthesize)
import           Syntax (Term, Name)
import           AnnotatedProgram
import qualified Syntax
import           AnnotatedDef

data Ann = 
    Memo | 
    Unfold
    deriving (Show, Eq)

-- Goals
data AnnG a
  = Term a :=: Term a
  | Conjunction (AnnG a) (AnnG a) [AnnG a] -- a list of conjuncts: at least 2 conjuncts should be present
  | Disjunction (AnnG a) (AnnG a) [AnnG a] -- a list of disjuncts: at least 2 disjuncts should be present
  | Fresh a (AnnG a)
  | Invoke Name [Term a] Ann
  | Delay (AnnG a)  deriving (Eq)


freshVars :: [a] -> AnnG a -> ([a], AnnG a)
freshVars names (Fresh name goal) = freshVars (name : names) goal
freshVars names goal = (reverse names, goal)

instance Show a => Show (AnnG a) where
  show (t1 :=:  t2) = printf "%s = %s" (show t1) (show t2)
  show (Conjunction x y gs) = printf "(%s)" (intercalate " /\\ " $ show <$> (x : y : gs))
  show (Disjunction x y gs) = printf "(%s)" (intercalate " \\/ " $ show <$> (x : y : gs))
  show (Fresh name g) =
    let (names, goal) = freshVars [name] g in
    printf "fresh %s (%s)" (unwords $ map show names) (show goal)
  show (Invoke name ts ann) =
    printf "%s %s %s" (show ann) name (unwords $ map (parenthesize . show) ts)
  show (Delay g) = printf "Delay (%s)" (show g)

annotateInvokesPr :: (AnnotatedProgram Syntax.G String) -> (AnnotatedProgram AnnG String)
annotateInvokesPr (AnnotatedProgram defs goal) =
    AnnotatedProgram (map annotateInvokesDef defs) (annotateInvokes goal)

annotateInvokesDef :: (AnnotatedDef Syntax.G String) -> (AnnotatedDef AnnG String)
annotateInvokesDef (AnnotatedDef name args body annotations) =
    AnnotatedDef name args (annotateInvokes body) annotations

annotateInvokes :: (Syntax.G String) -> (AnnG String)
annotateInvokes (Syntax.Conjunction g1 g2 gl) = 
    Conjunction (annotateInvokes g1) (annotateInvokes g2) (map annotateInvokes gl)
annotateInvokes (Syntax.Disjunction g1 g2 gl) = 
    Disjunction (annotateInvokes g1) (annotateInvokes g2) (map annotateInvokes gl)
annotateInvokes (Syntax.Fresh x body) =
    Fresh x (annotateInvokes body)
annotateInvokes (Syntax.Delay body) =
    Delay (annotateInvokes body)
annotateInvokes (Syntax.Invoke name terms) =
    Invoke name terms Unfold 
annotateInvokes (a Syntax.:=: b) = (a :=: b)