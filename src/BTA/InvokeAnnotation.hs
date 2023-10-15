{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances    #-}

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

annotateInvokes :: Ann -> Syntax.G t -> AnnG Term t
annotateInvokes ann (Syntax.Conjunction g1 g2 gl) = 
  Conjunction (annotateInvokes ann g1) (annotateInvokes ann g2) (map (annotateInvokes ann) gl)
annotateInvokes ann (Syntax.Disjunction g1 g2 gl) = 
  Disjunction (annotateInvokes ann g1) (annotateInvokes ann g2) (map (annotateInvokes ann) gl)
annotateInvokes ann (Syntax.Fresh x body) =
  Fresh x (annotateInvokes ann body)
annotateInvokes ann (Syntax.Delay body) =
  Delay (annotateInvokes ann body)
annotateInvokes ann (Syntax.Invoke name terms) =
  Invoke name terms ann 
annotateInvokes ann (a Syntax.:=: b) = 
  a :=: b
  
convertToSimple :: AnnG Term t -> Syntax.G t 
convertToSimple (Conjunction g1 g2 gl) = 
  Syntax.Conjunction (convertToSimple g1) (convertToSimple g2) (map convertToSimple gl)
convertToSimple (Disjunction g1 g2 gl) = 
  Syntax.Disjunction (convertToSimple g1) (convertToSimple g2) (map convertToSimple gl)
convertToSimple (Fresh x body) =
  Syntax.Fresh x $ convertToSimple body
convertToSimple (Delay body) =
  Syntax.Delay $ convertToSimple body
convertToSimple (Invoke name terms _) =
  Syntax.Invoke name terms 
convertToSimple (a :=: b) = 
  a Syntax.:=: b
  
infix  8 :=:
infixr 7 &&&
infixr 6 |||
infix  8 ===

(===) :: t a -> t a -> AnnG t a
(===) = (:=:)

(|||) :: AnnG t a -> AnnG t a -> AnnG t a
(|||) g1 g2 = goalFromList Disjunction [g1, g2]

(&&&) :: AnnG t a -> AnnG t a -> AnnG t a
(&&&) g1 g2 = goalFromList Conjunction [g1, g2]
