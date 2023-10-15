{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BTA.InvokeAnnotation where


import           Data.List          (intercalate)
import           Text.Printf        (printf)
import           Util.Miscellaneous (parenthesize)
import           Syntax             (Term, Name, goalFromList, Dot, dot)
import qualified Syntax

data Ann 
  = Memo 
  | Unfold
  deriving (Show, Eq, Ord)
  
instance Dot Ann where
  dot = show

data AnnG termType a
  = termType a :=: termType a
  | Conjunction (AnnG termType a) (AnnG termType a) [AnnG termType a]
  | Disjunction (AnnG termType a) (AnnG termType a) [AnnG termType a]
  | Fresh a (AnnG termType a)
  | Invoke Name [termType a] Ann
  | Delay (AnnG termType a)  
  deriving (Eq, Ord, Functor)


unsafeAnnConj :: [AnnG t a] -> AnnG t a
unsafeAnnConj = goalFromList Conjunction

unsafeAnnDisj :: [AnnG t a] -> AnnG t a
unsafeAnnDisj = goalFromList Disjunction

freshVars :: [a] -> AnnG termType a -> ([a], AnnG termType a)
freshVars names (Fresh name goal) = freshVars (name : names) goal
freshVars names goal = (reverse names, goal)

instance (Show a, Show (termType a)) => Show (AnnG termType a) where
  show (t1 :=: t2) = printf "%s == %s" (show t1) (show t2)
  show (Conjunction x y gs) = printf "(%s)" (intercalate " & " $ show <$> (x : y : gs))
  show (Disjunction x y gs) = printf "(%s)" (intercalate " | " $ show <$> (x : y : gs))
  show (Fresh name g) =
    let (names, goal) = freshVars [name] g in
    printf "(fresh %s in (%s))" (intercalate ", " $ map show names) (show goal)
  show (Invoke name ts ann) | null ts =
    printf "%s %s []" (show ann) name
  show (Invoke name ts ann) =
    printf "%s %s %s" (show ann) name (unwords $ map (parenthesize . show) ts)
  show (Delay g) = printf "Delay (%s)" (show g)
  
instance {-# OVERLAPPING #-} (Show (termType String)) => Show (AnnG termType String) where
  show (t1 :=: t2) = printf "%s == %s" (show t1) (show t2)
  show (Conjunction x y gs) = printf "(%s)" (intercalate " & " $ show <$> (x : y : gs))
  show (Disjunction x y gs) = printf "(%s)" (intercalate " | " $ show <$> (x : y : gs))
  show (Fresh name g) =
    let (names, goal) = freshVars [name] g in
    printf "(fresh %s in (%s))" (intercalate ", " names) (show goal)
  show (Invoke name ts ann) | null ts =
    printf "%s %s []" (show ann) name
  show (Invoke name ts ann) =
    printf "%s %s %s" (show ann) name (unwords $ map (parenthesize . show) ts)
  show (Delay g) = printf "Delay (%s)" (show g)
  
instance (Dot a, Dot (termType a)) => Dot (AnnG termType a) where
  dot (t1 :=: t2) = printf "%s = %s" (dot t1) (dot t2)
  dot (Conjunction x y gs) = printf "(%s)" (intercalate " /\\ " $ dot <$> (x : y : gs))
  dot (Disjunction x y gs) = printf "(%s)" (intercalate " \\/ " $ dot <$> (x : y : gs))
  dot (Fresh name g) =
    let (names, goal) = freshVars [name] g in
    printf "(fresh %s (%s))" (unwords $ map dot names) (dot goal)
  dot (Invoke name ts ann) =
    printf "%s %s %s" (dot ann) name (unwords $ map (parenthesize . dot) ts)
  dot (Delay g) = printf "Delay (%s)" (dot g)

annotateInvokes :: Syntax.G String -> AnnG Term String
annotateInvokes (Syntax.Conjunction g1 g2 gl) = 
  Conjunction (annotateInvokes g1) (annotateInvokes g2) (map annotateInvokes gl)
annotateInvokes (Syntax.Disjunction g1 g2 gl) = 
  Disjunction (annotateInvokes g1) (annotateInvokes g2) (map annotateInvokes gl)
annotateInvokes (Syntax.Fresh x body) =
  Fresh x (annotateInvokes body)
annotateInvokes (Syntax.Delay body) =
  Delay (annotateInvokes body)
annotateInvokes (Syntax.Invoke name terms) =
  Invoke name terms Memo 
annotateInvokes (a Syntax.:=: b) = 
  a :=: b