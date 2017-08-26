module SubParserSpec (tests) where

import Test.Hspec
import SubParser
import SubContainer
import Data.List
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import Text.Parsec.Error

import Common

tests =
  describe "SubParse" $ do
    -- it "Dummy run" $ do
    --   runParser parseSubFile () "" "" `shouldSatisfy` (not . isLeft)

    it "Parses a control sequence" $ do
      runParser (control $ string "ABCD") () "" "{ABCD}" `shouldBe` Right "ABCD"

    it "Parses a number in base 10" $ do
      runParser number () "" "393" `shouldBe` Right 393
