module SrtParserSpec (tests) where

import Test.Hspec
import SrtParser
import Data.List
import Text.ParserCombinators.Parsec

dump :: TextTree -> String
dump = dumpi 0
dumpi :: Int -> TextTree -> String
dumpi n (LeafText s) = replicate n ' ' ++  s ++ "\n"
dumpi n (TextTag t _) = replicate n ' ' ++ "t:\n" ++ dumpi (n + 1) t
dumpi n (TextNode ns) = foldl' (\l -> \r -> l ++ (dumpi n r)) "" ns

dumpResult (Right t) = dump t
dumpResult (Left e) = show e


tests =
  describe "SrtParse" $ do
    it "sanity checks" $ do
      (True == False) `shouldBe` False

    it "Parses a timestamp" $ do
      runParser
        parseTimeStamp defaultState "" "10:11:38,100" `shouldBe` (Right 36660100)
