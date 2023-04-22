module Ch5.FormulasParserSpec where

import Test.Hspec
import Text.Parsec
import Data.Text

import Ch5.FormulasParser

spec :: Spec
spec = do
  describe "2 + 3 * 4" $ do
    it "should evaluate to 14" $ do
      parseEval "2 + 3 * 4" `shouldBe` Right 14
  describe "(2 + 3) * 4" $ do
    it "should evaluate to 20" $ do
      parseEval "(2 + 3) * 4" `shouldBe` Right 20
  describe "(2 + 13) * (4 + 5)" $ do
    it "should evaluate to 135" $ do
      parseEval "(2 + 13) * (4 + 5)" `shouldBe` Right 135
  where
    parseEval expr = eval <$> parse parseExpr "" (pack expr)