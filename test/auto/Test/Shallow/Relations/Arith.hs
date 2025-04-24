{-# LANGUAGE ApplicativeDo #-}
module Test.Shallow.Relations.Arith(lto, leo, gto, geo, minmaxo, mino, maxo, similaro) where


import Test.Shallow.Types.Nat
import Shallow.Core.Kanren

lto :: (Kanren rel) => L Natural rel -> L Natural rel -> Relation rel
lto = relation2 "lto" $ \l g -> conde 
    [ fresh $ \g' -> do
        l <=> zro
        g <=> suc g'
        pure ()
    , fresh2 $ \l' g' -> do
        l <=> suc l'
        g <=> suc g'
        call $ lto l' g'
        pure ()
    ]

leo :: (Kanren rel) => L Natural rel -> L Natural rel -> Relation rel
leo = relation2 "leo" $ \l g -> conde [l <=> g, call $ l `lto` g]

gto :: (Kanren rel) => L Natural rel -> L Natural rel -> Relation rel
gto = relation2 "gto" $ \g l -> embed $ l `lto` g

geo :: (Kanren rel) => L Natural rel -> L Natural rel -> Relation rel
geo = relation2 "geo" $ \g l -> embed $ g `leo` l

minmaxo :: (Kanren rel) => L Natural rel -> L Natural rel -> L Natural rel -> L Natural rel -> Relation rel
minmaxo = relation4 "minmaxo" $ \x y mn mx -> conde
    [ do
        call $ x `leo` y
        mn <=> x
        mx <=> y
        pure ()
    , do
        call $ x `gto` y
        mn <=> y
        mx <=> x
        pure ()
    ]

mino :: (Kanren rel) => L Natural rel -> L Natural rel -> L Natural rel -> Relation rel
mino = relation3 "min" $ \x y mn -> fresh $ \mx -> embed $ minmaxo x y mn mx

maxo :: (Kanren rel) => L Natural rel -> L Natural rel -> L Natural rel -> Relation rel
maxo = relation3 "maxo" $ \x y mx -> fresh $ \mn -> embed $ minmaxo x y mn mx

similaro :: (Kanren rel) => L Natural rel -> L Natural rel -> Relation rel
similaro = relation2 "similaro" $ \x y -> conde [ x <=> y, x <=> suc y, y <=> suc x ]