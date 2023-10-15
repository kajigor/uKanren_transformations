module Test.Mode.Inst where

import Mode.Inst
import Test.Helper

freeToNothing :: Mode
freeToNothing = Mode {before = Free, after = Nothing}

freeToFree :: Mode
freeToFree = Mode {before = Free, after = Just Free}

freeToGround :: Mode
freeToGround = Mode {before = Free, after = Just Ground}

groundToNothing :: Mode
groundToNothing = Mode {before = Ground, after = Nothing}

groundToFree :: Mode
groundToFree = Mode {before = Ground, after = Just Free}

groundToGround :: Mode
groundToGround = Mode {before = Ground, after = Just Ground}

unit_identicalBeforeMode :: IO ()
unit_identicalBeforeMode = do
  test2 identicalBeforeMode freeToNothing freeToNothing True
  test2 identicalBeforeMode freeToFree freeToNothing True
  test2 identicalBeforeMode freeToNothing freeToGround True
  test2 identicalBeforeMode freeToFree freeToGround True

  test2 identicalBeforeMode groundToNothing groundToNothing True
  test2 identicalBeforeMode groundToFree groundToNothing True
  test2 identicalBeforeMode groundToNothing groundToGround True
  test2 identicalBeforeMode groundToFree groundToGround True

  test2 identicalBeforeMode groundToNothing freeToNothing  False
  test2 identicalBeforeMode freeToFree groundToNothing False
  test2 identicalBeforeMode groundToNothing freeToGround False
  test2 identicalBeforeMode freeToFree groundToGround False

args0, args1, args2, args3, longArgs :: [(String, Mode)]
args0 = [("x", freeToNothing), ("y", freeToNothing)]
args1 = [("x", freeToNothing), ("y", groundToNothing)]
args2 = [("x", groundToNothing), ("y", freeToNothing)]
args3 = [("x", groundToNothing), ("y", groundToNothing)]
longArgs = [("x", groundToNothing), ("y", freeToNothing), ("z", freeToGround)]

unit_compatibleBeforeModes :: IO ()
unit_compatibleBeforeModes = do
  test2 compatibleBeforeModes args0 args0 True
  test2 compatibleBeforeModes args0 args1 False
  test2 compatibleBeforeModes args0 args2 False
  test2 compatibleBeforeModes args0 args3 False

  test2 compatibleBeforeModes args1 args0 True
  test2 compatibleBeforeModes args1 args1 True
  test2 compatibleBeforeModes args1 args2 False
  test2 compatibleBeforeModes args1 args3 False

  test2 compatibleBeforeModes args2 args0 True
  test2 compatibleBeforeModes args2 args1 False
  test2 compatibleBeforeModes args2 args2 True
  test2 compatibleBeforeModes args2 args3 False

  test2 compatibleBeforeModes args3 args0 True
  test2 compatibleBeforeModes args3 args1 True
  test2 compatibleBeforeModes args3 args2 True
  test2 compatibleBeforeModes args3 args3 True

  test2 compatibleBeforeModes longArgs longArgs True
  test2 compatibleBeforeModes args0 longArgs False
  test2 compatibleBeforeModes args1 longArgs False
  test2 compatibleBeforeModes args2 longArgs False
  test2 compatibleBeforeModes args3 longArgs False

unit_identicalBeforeModes :: IO ()
unit_identicalBeforeModes = do
  test2 identicalBeforeModes args0 args0 True
  test2 identicalBeforeModes args0 args1 False
  test2 identicalBeforeModes args0 args2 False
  test2 identicalBeforeModes args0 args3 False

  test2 identicalBeforeModes args1 args0 False
  test2 identicalBeforeModes args1 args1 True
  test2 identicalBeforeModes args1 args2 False
  test2 identicalBeforeModes args1 args3 False

  test2 identicalBeforeModes args2 args0 False
  test2 identicalBeforeModes args2 args1 False
  test2 identicalBeforeModes args2 args2 True
  test2 identicalBeforeModes args2 args3 False

  test2 identicalBeforeModes args3 args0 False
  test2 identicalBeforeModes args3 args1 False
  test2 identicalBeforeModes args3 args2 False
  test2 identicalBeforeModes args3 args3 True

  test2 identicalBeforeModes longArgs longArgs True
  test2 identicalBeforeModes args0 longArgs False
  test2 identicalBeforeModes args1 longArgs False
  test2 identicalBeforeModes args2 longArgs False
  test2 identicalBeforeModes args3 longArgs False
