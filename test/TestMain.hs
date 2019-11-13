module TestMain (
    main
) where

import qualified OCanrenizeTest as OC
import qualified ResidualizeTest as R
import qualified DrivingTest as D
import qualified BridgeTest as BT
import qualified PrintingTest as PT
import qualified EmbedTest as ET
import qualified SomeTest as ST

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
