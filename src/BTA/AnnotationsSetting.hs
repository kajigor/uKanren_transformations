
module BTA.AnnotationsSetting where

import BTA.Inequalities
import BTA.TerminationCheck
import AnnotatedProgram
import AnnotatedDef
import qualified Data.Map as Map
import qualified InvokeAnnotation as Inv
import BTA.SizeConversion
import BTA.NormalizeAnnotated
import Syntax
import Debug.Trace
import qualified Data.Set as Set


setAnnotations :: AnnotatedProgram Inv.AnnG String -> AnnotatedProgram Inv.AnnG String
setAnnotations program@(AnnotatedProgram defs goal) = 
    AnnotatedProgram (until (\x -> (Set.fromList $ goDefsAnnotations goal x) == (Set.fromList x)) (goDefsAnnotations goal) defs) goal

goDefsAnnotations :: Inv.AnnG String -> [AnnotatedDef Inv.AnnG String] -> [AnnotatedDef Inv.AnnG String]
goDefsAnnotations goal defs =
    foldl (\curDefs def -> goDefAnnotations def (curDefs ++ (drop (length curDefs + 1) defs)) goal : curDefs) [] defs

goDefAnnotations :: AnnotatedDef Inv.AnnG String -> [AnnotatedDef Inv.AnnG String] -> Inv.AnnG String -> AnnotatedDef Inv.AnnG String
goDefAnnotations def@(AnnotatedDef name args body annotations) otherDefs goal = 
    AnnotatedDef name args (goBodyAnnotations body (\x -> AnnotatedProgram ((AnnotatedDef name args x annotations) : otherDefs) goal)) annotations


check :: AnnotatedProgram Inv.AnnG String -> Bool 
check program = 
    let abstractProgram@(AnnotatedProgram defs goal) = makeNormal $ convert program in 
    let mapDefs = Map.fromList $ zip (map getName defs) defs in
    let mapConditions = go defs mapDefs Map.empty in 
    terminationCheck abstractProgram mapConditions mapDefs
    
severalGoalsAnnotations :: Inv.AnnG String -> Inv.AnnG String -> [Inv.AnnG String] -> (Inv.AnnG String -> Inv.AnnG String -> [Inv.AnnG String] -> Inv.AnnG String) -> (Inv.AnnG String -> AnnotatedProgram Inv.AnnG String) -> Inv.AnnG String
severalGoalsAnnotations g1 g2 lstG constructor recovery = 
    let g1New = goBodyAnnotations g1 (\x -> recovery $ constructor x g2 lstG) in 
    let g2New = goBodyAnnotations g2 (\x -> recovery $ constructor g1New x lstG) in 
    let lstGNew = foldl (\curLst curG -> curLst ++ [goBodyAnnotations curG (\x -> recovery $ constructor g1New g2New $ x : (curLst ++ (drop (length curLst + 1) lstG)))]) [] lstG in
    constructor g1New g2New lstGNew

goBodyAnnotations :: Inv.AnnG String -> (Inv.AnnG String -> AnnotatedProgram Inv.AnnG String) -> Inv.AnnG String
goBodyAnnotations goal@(Inv.Conjunction g1 g2 lstG) recovery = 
    severalGoalsAnnotations g1 g2 lstG Inv.Conjunction recovery
goBodyAnnotations goal@(Inv.Disjunction g1 g2 lstG) recovery = 
    severalGoalsAnnotations g1 g2 lstG Inv.Disjunction recovery
goBodyAnnotations goal@(Inv.Fresh x g) recovery = 
    Inv.Fresh x $ goBodyAnnotations g (\g1 -> recovery (Inv.Fresh x g1))
goBodyAnnotations goal@(Inv.Delay g) recovery = 
    Inv.Delay $ goBodyAnnotations g (\g1 -> recovery (Inv.Delay g1))
goBodyAnnotations goal@(term1 Inv.:=: term2) recovery = 
    goal
goBodyAnnotations goal@(Inv.Invoke name terms ann) recovery | ann == Inv.Memo && (check $ recovery (Inv.Invoke name terms Inv.Unfold)) = (Inv.Invoke name terms Inv.Unfold)
                                                            | otherwise = goal
