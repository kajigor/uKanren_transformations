{-# LANGUAGE ScopedTypeVariables #-}

module Test.Generalization where

import           Test.Helper    (test2)
import           Generalization
import           Syntax
import qualified Data.Map as Map

gen1 :: Generalizer
gen1 = Map.empty

gen2 :: Generalizer
gen2 = Map.empty

gen1' = []
gen2' = []

freshNames :: [Int]
freshNames = [10..15]

vars :: [Ts]
vars@[x, y, z] = map V [1, 2, 3]

c :: [Ts] -> Ts
c = C ""

inv :: [Ts] -> G S
inv = Invoke ""

gx = inv [x]
gy = inv [y]
gz = inv [z]
gxx = inv [x, x]
gxy = inv [x, y]
gxxy = inv [x, x, y]
gxyy = inv [x, y, y]

unit_generalizeTerm = do
    let function = test2 (generalize freshNames gen1 gen2)
    function x x (x, gen1, gen2, freshNames)
    function y z (let (n:t) = freshNames in (V n, Map.fromList $ (n, y) : gen1', Map.fromList $ (n, z) : gen2', t))
    function (c [x, x])
             (c [x, y])
             (let (n:t) = freshNames in (c [x, V n], Map.fromList $ (n, x) : gen1', Map.fromList $ (n, y) : gen2', t))
    function (c [x, x, y])
             (c [x, y, y])
             (let (n:t) = freshNames in (c [x, V n, y], Map.fromList $ (n, x) : gen1', Map.fromList $ (n, y) : gen2', t))
    function (c [c [], x, y])
             (c [x, c [], y])
             (let (n:m:t) = freshNames in (c [V n, V m, y], Map.fromList $ (m, x) : (n, c []) : gen1', Map.fromList $ (m, c []) : (n, x) : gen2', t))

unit_generalizeGoal = do
    let function = test2 (generalize freshNames gen1 gen2)
    function gx gx (gx, gen1, gen2, freshNames)
    function gy gz (let (n:t) = freshNames in (inv [V n], Map.fromList $ (n, y) : gen1', Map.fromList $ (n, z) : gen2', t))
    function gxx
             gxy
             (let (n:t) = freshNames in (inv [x, V n], Map.fromList $ (n, x) : gen1', Map.fromList $ (n, y) : gen2', t))
    function gxxy
             gxyy
             (let (n:t) = freshNames in (inv [x, V n, y], Map.fromList $ (n, x) : gen1', Map.fromList $ (n, y) : gen2', t))
    function (inv [c [], x, y])
             (inv [x, c [], y])
             (let (n:m:t) = freshNames in (inv [V n, V m, y], Map.fromList $ (m, x) : (n, c []) : gen1', Map.fromList $ (m, c []) : (n, x) : gen2', t))

unit_generalizeGoals = do
    let function = test2 (generalize freshNames gen1 gen2)
    function [gx, gy] [gx, gy] ([gx, gy], gen1, gen2, freshNames)
    function [gx, gy]
             [gy, gx]
             (let (n:m:t) = freshNames in ([inv [V n], inv [V m]], Map.fromList $ (m, y) : (n, x) : gen1', Map.fromList $ (m, x) : (n, y) : gen2', t))
    function [gxxy, gxyy, gx]
             [gxyy, gxxy, gy]
             (let (n:m:p:t) = freshNames in
              ( [inv [x, V n, y], inv [x, V m, y], inv [V p]]
              , Map.fromList $ [(p, x), (m, y), (n, x)] ++ gen1'
              , Map.fromList $ [(p, y), (m, x), (n, y)] ++ gen2'
              , t
              ))
    function [inv [x, c [x, y]], inv [c [z, z], c [x, x]]]
             [inv [c [x], c [x, z]], inv [c [z, z], y]]
             (let (n:m:p:t) = freshNames in
              ( [inv [V n, c [x, V m]], inv [c [z, z], V p]]
              , Map.fromList $ [(p, c [x, x]), (m, y), (n, x)] ++ gen1'
              , Map.fromList $ [(p, y), (m, z), (n, c [x])] ++ gen2'
              , t
              ))