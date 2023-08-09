
module BTA.AnnotationType where

data AnnotationType 
    = Static 
    | Dynamic
    | TypeC String [AnnotationType] 
    deriving (Eq, Ord)

instance Show AnnotationType where
    show Static = "static"
    show Dynamic = "dynamic"
    show (TypeC name args) = name ++ "(" ++ (unwords $ map show args) ++ ")"