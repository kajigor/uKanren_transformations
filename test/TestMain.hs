module TestMain (
    main
) where

import qualified BridgeTest          as BT
import qualified DrivingTest         as D
import qualified EmbedTest           as ET
import qualified OCanrenizeTest      as OC
import qualified PrintingTest        as PT
import qualified ResidualizationTest as R
import qualified SomeTest            as ST

main :: IO ()
main = do
  --PT.main
  --OC.main

  --print ET.test

  --print ET.g1
  --print ET.g2
  --print ET.test'

  --BT.test
  --BT.testBig
  --BT.testEq

  OC.main
  --ST.test
  --ST.test'
