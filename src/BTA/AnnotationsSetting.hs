
module BTA.AnnotationsSetting where

import           Debug.Trace
import           Syntax                (Term)
import           BTA.AnnotatedDef
import           BTA.AnnotatedProgram
import           BTA.InvokeAnnotation
import           BTA.Inequalities
import           BTA.NormalizeAnnotated
import           BTA.SizeConversion
import           BTA.TerminationCheck
import qualified Data.Map as Map
import qualified Data.Set as Set


setAnnotations :: AnnotatedProgram (AnnG Term) String -> AnnotatedProgram (AnnG Term) String
setAnnotations program@(AnnotatedProgram defs goal) = 
    AnnotatedProgram (until (\x -> (Set.fromList $ goDefsAnnotations goal x) == (Set.fromList x)) (goDefsAnnotations goal) defs) goal

goDefsAnnotations :: AnnG Term String -> [AnnotatedDef (AnnG Term) String] -> [AnnotatedDef (AnnG Term) String]
goDefsAnnotations goal defs =
    foldl (\curDefs def -> goDefAnnotations def (curDefs ++ (drop (length curDefs + 1) defs)) goal : curDefs) [] defs

goDefAnnotations :: AnnotatedDef (AnnG Term) String -> [AnnotatedDef (AnnG Term) String] -> AnnG Term String -> AnnotatedDef (AnnG Term) String
goDefAnnotations def@(AnnotatedDef name args body annotations) otherDefs goal = 
    AnnotatedDef name args (goBodyAnnotations body (\x -> AnnotatedProgram (AnnotatedDef name args x annotations : otherDefs) goal)) annotations


check :: AnnotatedProgram (AnnG Term) String -> Bool 
check program = 
    let abstractProgram@(AnnotatedProgram defs goal) = makeNormal $ convert program in 
    let mapDefs = Map.fromList $ zip (map getName defs) defs in
    let mapConditions = go defs mapDefs Map.empty in 
    terminationCheck abstractProgram mapConditions mapDefs
    
severalGoalsAnnotations :: AnnG Term String -> AnnG Term String -> [AnnG Term String] -> (AnnG Term String -> AnnG Term String -> [AnnG Term String] -> AnnG Term String) -> (AnnG Term String -> AnnotatedProgram (AnnG Term) String) -> AnnG Term String
severalGoalsAnnotations g1 g2 lstG constructor recovery = 
    let g1New = goBodyAnnotations g1 (\x -> recovery $ constructor x g2 lstG) in 
    let g2New = goBodyAnnotations g2 (\x -> recovery $ constructor g1New x lstG) in 
    let lstGNew = foldl (\curLst curG -> curLst ++ [goBodyAnnotations curG (\x -> recovery $ constructor g1New g2New $ curLst ++ [x] ++ (drop (length curLst + 1) lstG))]) [] lstG in
    constructor g1New g2New lstGNew

goBodyAnnotations :: AnnG Term String -> (AnnG Term String -> AnnotatedProgram (AnnG Term) String) -> AnnG Term String
goBodyAnnotations goal@(Conjunction g1 g2 lstG) recovery = 
    severalGoalsAnnotations g1 g2 lstG Conjunction recovery
goBodyAnnotations goal@(Disjunction g1 g2 lstG) recovery = 
    severalGoalsAnnotations g1 g2 lstG Disjunction recovery
goBodyAnnotations goal@(Fresh x g) recovery = 
    Fresh x $ goBodyAnnotations g (\g1 -> recovery (Fresh x g1))
goBodyAnnotations goal@(Delay g) recovery = 
    Delay $ goBodyAnnotations g (\g1 -> recovery (Delay g1))
goBodyAnnotations goal@(term1 :=: term2) recovery = 
    goal
goBodyAnnotations goal@(Invoke name terms ann) recovery | ann == Memo && (check $ recovery $ Invoke name terms Unfold) = Invoke name terms Unfold
                                                        | otherwise = goal
