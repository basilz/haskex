module Ch6.TestMiniSpec where

import Ch6.MiniWhile
import Data.Foldable     (for_)
import qualified Data.Map as M
import Test.Hspec

spec :: Spec
spec = describe "MiniWhile" $ for_ cases test
  where 
    test (input, expected) = it description assertion
      where
        description = "parse " <> show input
        assertion = parseEval input >>= flip shouldBe expected

    cases = [
      ("x := 3",                                        Right (M.singleton "x" 3)),
      ("x := 2; x := if x <= 3 then 3 else 1",          Right (M.singleton "x" 3)),
      ("x := if 5 <= 3 then 3 else 1",                  Right (M.singleton "x" 1)),
      ("x := 100; while x > 3 do x := x - 1 done",      Right (M.singleton "x" 3)),
      ("x := 100; while not x <= 3 do x := x - 1 done", Right (M.singleton "x" 3)),
      ("x:= 0; y:= 5;while x <= 3 do y:= (y * 5); x:= (x + 1) done; y:= if y > 10000 then 10000 else y fi", 
                                                        Right (M.fromList [("x", 4), ("y", 3125)]))]
