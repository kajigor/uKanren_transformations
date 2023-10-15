
module Test.BTA.AbstractProgram where

import           Test.Helper 
import           BTA.SizeConversion 
import           BTA.InvokeAnnotation 
import           BTA.AnnotatedProgram
import           BTA.AnnotatedDef
import qualified Syntax                 as S
import           BTA.AnnotationType
import           Util.Miscellaneous     (mapLeft)
import qualified Parser.Parser          as Parser
import qualified Parser.AnnotatedParser as AnnotatedParser
import           Data.Either
import qualified Data.Map               as Map
import           Data.Group

getAnnotationParser :: (String -> IO (Either String (AnnotatedProgram S.G S.X)))
getAnnotationParser input = do 
  res <- Parser.parseImports AnnotatedParser.parseProgramWithImports input
  return $ mapLeft show res

sumAnn :: AnnotatedProgram S.G S.X
sumAnn = AnnotatedProgram 
    [ AnnotatedDef "fail" [] (S.Invoke "fail" []) []
    , AnnotatedDef "addo" ["x", "y", "z"] (
        S.Disjunction (
            S.Conjunction (
                S.V "x" S.:=: S.C "Zero" []
            ) (
                S.V "z" S.:=: S.V "y"
            ) []
        ) (
            S.Fresh "x'" $ 
                S.Conjunction (
                    S.V "x" S.:=: S.C "Succ" [S.V "x'"]
                ) (
                    S.Invoke "addo" [S.V "x'", S.C "Succ" [S.V "y"], S.V "z"]
                ) []
        ) []
    ) [Static, Dynamic, Dynamic]
    , AnnotatedDef "evalo" ["fm", "r"] (
        S.Disjunction (
            S.V "fm" S.:=: S.C "Num" [S.V "r"]
        ) (
            S.Fresh "x" $ S.Fresh "y" $ S.Fresh "xr" $ S.Fresh "yr" $ 
                S.Conjunction (
                    S.Invoke "evalo" [S.V "x", S.V "xr"]
                ) (
                    S.Invoke "evalo" [S.V "y", S.V "yr"]
                ) [
                    S.V "fm" S.:=: S.C "Sum" [S.V "x", S.V "y"]
                    , S.Invoke "addo" [S.V "xr", S.V "yr", S.V "r"]
                ]
        ) []
    ) [Static, Dynamic] 
    ] (S.Fresh "y" $ S.Invoke "addo" [S.C "Zero" [], S.V "y", S.C "Succ" [S.C "Succ" [S.C "Zero" []]]])

sumAnnInvoke :: AnnotatedProgram (AnnG S.Term) S.X
sumAnnInvoke = AnnotatedProgram 
    [ AnnotatedDef "fail" [] (Invoke "fail" [] Memo) []
    , AnnotatedDef "addo" ["x", "y", "z"] (
        Disjunction (
            Conjunction (
                S.V "x" :=: S.C "Zero" []
            ) (
                S.V "z" :=: S.V "y"
            ) []
        ) (
            Fresh "x'" $ 
                Conjunction (
                    S.V "x" :=: S.C "Succ" [S.V "x'"]
                ) (
                    Invoke "addo" [S.V "x'", S.C "Succ" [S.V "y"], S.V "z"] Memo
                ) []
        ) []
    ) [Static, Dynamic, Dynamic]
    , AnnotatedDef "evalo" ["fm", "r"] (
        Disjunction (
            S.V "fm" :=: S.C "Num" [S.V "r"]
        ) (
            Fresh "x" $ Fresh "y" $ Fresh "xr" $ Fresh "yr" $ 
                Conjunction (
                    Invoke "evalo" [S.V "x", S.V "xr"] Memo
                ) (
                    Invoke "evalo" [S.V "y", S.V "yr"] Memo
                ) [
                    S.V "fm" :=: S.C "Sum" [S.V "x", S.V "y"]
                    , (Invoke "addo" [S.V "xr", S.V "yr", S.V "r"] Memo)
                ]
        ) []
    ) [Static, Dynamic]
    ] (Fresh "y" $ Invoke "addo" [S.C "Zero" [], S.V "y", S.C "Succ" [S.C "Succ" [S.C "Zero" []]]] Memo)

unit_parseAnnotations = do 
    program <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/sum.mk"
    program @?= (Right sumAnn :: Either String (AnnotatedProgram S.G S.X))
    program1 <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/prop.mk"
    let annotations = either (*> [[]]) (map getAnnotations . getDefs) program1
    annotations @?= [[], [Static, Static, Dynamic], [Static, Static, Dynamic], [Static, Static], [Static, Static, Dynamic], [Static, Static, Dynamic], [Static, Static, Dynamic]] 
    program2 <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/incorrect.mk"
    isLeft program2 @?= True
    program3 <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/num.mk"
    let annotations3 = either (*> [[]]) (map getAnnotations . getDefs) program3
    annotations3 @?= [[], [Dynamic], [Static, Dynamic, Dynamic], [Static, Static, Dynamic], [Static, Static, Dynamic], [Dynamic, Static, Static], [Static, Static, Dynamic], [Static, Dynamic, Static]] 

oneAnnInvoke = Fresh "h" $ Fresh "t" $ Fresh "n'" $
    Disjunction (
        Conjunction (
            S.V "n" :=: S.C "Zero" []
        ) (
            S.V "s" :=: S.C "Cons" [S.V "h", S.V "t"]
        ) [
            S.V "v" :=: S.V "h"
        ]
    ) (
        Conjunction (
            S.V "s" :=: S.C "Cons" [S.V "h", S.V "t"]
        ) (
            Delay (Invoke "elemo" [S.V "n'", S.V "t", S.V "v"] Memo)
        ) [
            S.V "n" :=: S.C "Succ" [S.V "n'"]
        ]
    ) []

unit_annotateInvokes = do
    let sumAnnInvoke1 = annotateInvokesPr sumAnn
    sumAnnInvoke1 @?= sumAnnInvoke 
    program <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/prop.mk"
    let propAnnInvokeDefs = either (*> []) (map getBody . getDefs . annotateInvokesPr) program
    (propAnnInvokeDefs !! 6) @?= oneAnnInvoke

sumAnnAbstract :: AnnotatedProgram (AnnG AbstractTerm) S.X
sumAnnAbstract = AnnotatedProgram 
    [ AnnotatedDef "fail" [] (Invoke "fail" [] Memo) []
    , AnnotatedDef "addo" ["x", "y", "z"] (
        Disjunction (
            Conjunction (
                Sum 0 (Map.fromList [("x", 1)]) :=: Sum 1 Map.empty
            ) (
                Sum 0 (Map.fromList [("z", 1)]) :=: Sum 0 (Map.fromList [("y", 1)])
            ) []
        ) (
            Fresh "x'" $ 
                Conjunction (
                    Sum 0 (Map.fromList [("x", 1)]) :=: Sum 1 (Map.fromList [("x'", 1)])
                ) (
                    Invoke "addo" [Sum 0 (Map.fromList [("x'", 1)]), Sum 1 (Map.fromList [("y", 1)]), Sum 0 (Map.fromList [("z", 1)])] Memo
                ) []
        ) []
    ) [Static, Dynamic, Dynamic]
    , AnnotatedDef "evalo" ["fm", "r"] (
        Disjunction (
            Sum 0 (Map.fromList [("fm", 1)]) :=: Sum 1 (Map.fromList [("r", 1)])
        ) (
            Fresh "x" $ Fresh "y" $ Fresh "xr" $ Fresh "yr" $ 
                Conjunction (
                    Invoke "evalo" [Sum 0 (Map.fromList [("x", 1)]), Sum 0 (Map.fromList [("xr", 1)])] Memo
                ) (
                    Invoke "evalo" [Sum 0 (Map.fromList [("y", 1)]), Sum 0 (Map.fromList [("yr", 1)])] Memo
                ) [
                    Sum 0 (Map.fromList [("fm", 1)]) :=: Sum 1 (Map.fromList [("x", 1), ("y", 1)])
                    , Invoke "addo" [Sum 0 (Map.fromList [("xr", 1)]), Sum 0 (Map.fromList [("yr", 1)]), Sum 0 (Map.fromList [("r", 1)])] Memo
                ]
        ) []
    ) [Static, Dynamic]
    ] (Fresh "y" $ Invoke "addo" [Sum 1 Map.empty, Sum 0 $ Map.fromList [("y", 1)], Sum 3 Map.empty] Memo)

appendoAbst :: (AnnG AbstractTerm) S.X
appendoAbst = 
    Disjunction (
        Conjunction (
            Sum 0 (Map.fromList [("x", 1)]) :=: Sum 1 Map.empty
        ) (
            Sum 0 (Map.fromList [("y", 1)]) :=: Sum 0 (Map.fromList [("xy", 1)])
        ) [] 
    )  (
        Fresh "h" $ Fresh "t" $ Fresh "ty" $ 
        Conjunction (
            Sum 0 (Map.fromList [("x", 1)]) :=: Sum 1 (Map.fromList [("h", 1), ("t", 1)])
        ) (
            Sum 0 (Map.fromList [("xy", 1)]) :=: Sum 1 (Map.fromList [("h", 1), ("ty", 1)])
        ) [
            Invoke "appendo" [Sum 0 (Map.fromList [("t", 1)]), Sum 0 (Map.fromList [("y", 1)]), Sum 0 (Map.fromList [("ty", 1)])] Memo
        ] 
    ) 
    [] 

reversoAbst :: (AnnG AbstractTerm) S.X
reversoAbst = 
    Disjunction (
        Conjunction (
            Sum 0 (Map.fromList [("x", 1)]) :=: Sum 1 Map.empty
        ) (
            Sum 0 (Map.fromList [("y", 1)]) :=: Sum 1 Map.empty
        ) []
    ) (
        Fresh "h" $ Fresh "t" $ Fresh "rt" $
            Conjunction (
                Sum 0 (Map.fromList [("x", 1)]) :=: Sum 1 (Map.fromList [("h", 1), ("t", 1)])
            ) (
                Invoke "reverso" [Sum 0 (Map.fromList [("t", 1)]), Sum 0 (Map.fromList [("rt", 1)])] Memo
            ) [
                Invoke "appendo" [Sum 0 (Map.fromList [("rt", 1)]), Sum 2 (Map.fromList [("h", 1)]), Sum 0 (Map.fromList [("y", 1)])] Memo
            ]
    ) [] 


unit_convert = do
    let sumAnnAbstract1 = convert sumAnnInvoke
    sumAnnAbstract1 @?= sumAnnAbstract
    program <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/list/list.mk"
    let propAnnAbstractDefs = either (*> []) (map getBody . getDefs . convert . annotateInvokesPr) program
    propAnnAbstractDefs !! 1 @?= appendoAbst
    propAnnAbstractDefs !! 2 @?= reversoAbst

sum_a_1 = Sum 1 (Map.fromList [("a", 1)])
sum_a_1_b_2 = Sum 1 (Map.fromList [("a", 1), ("b", 2)])
sum_a_2 = Sum 1 (Map.fromList [("a", 2)])
sum_a_1_2 = Sum 2 (Map.fromList [("a", 1)])

unit_terms_eq = do
    test2 (==) sum_a_1 sum_a_1 True
    test2 (==) sum_a_1 sum_a_1_b_2 False
    test2 (==) (Sum 1 (Map.fromList [("a", 1), ("c", 2)])) sum_a_1_b_2 False

unit_terms_weight = do
    sum_a_1_b_2 ~~ sum_a_1 @?= Sum 0 (Map.fromList [("b", 2)])
    sum_a_2 ~~ sum_a_1 @?= Sum 0 (Map.fromList [("a", 1)])
    sum_a_2 ~~ sum_a_1_2 @?= Sum (-1) (Map.fromList [("a", 1)])
    sum_a_1 ~~ sum_a_1 @?= Sum 0 (Map.empty :: Map.Map String Int)

unit_terms_less = do
    test2True isLess sum_a_1 sum_a_1_b_2
    test2True isLess sum_a_1 sum_a_2
    test2False isLess sum_a_1_2 sum_a_2
    test2False isLess sum_a_1 sum_a_1