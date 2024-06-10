
module BTA.AnnotationType where
import Syntax (Dot, dot)

data AnnotationType 
    = Static 
    | Dynamic
    | TypeC String [AnnotationType] 
    deriving (Eq, Ord)

instance Show AnnotationType where
    show Static = "static"
    show Dynamic = "dynamic"
    show (TypeC name args) = name ++ "(" ++ unwords (map show args) ++ ")"
    
instance Dot AnnotationType where
    dot Static = "static"
    dot Dynamic = "dynamic"
    dot (TypeC name args) = name ++ "(" ++ unwords (map dot args) ++ ")"