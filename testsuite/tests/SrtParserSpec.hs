module SrtParserSpec (tests) where

import Test.Hspec
import SrtParser
import SubContainer
import Data.List
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import Text.Parsec.Error

import Common

dump :: TextTree -> String
dump = dumpi 0
dumpi :: Int -> TextTree -> String
dumpi n (LeafText s) = replicate n ' ' ++  s ++ "\n"
dumpi n (TextTag t _) = replicate n ' ' ++ "t:\n" ++ dumpi (n + 1) t
dumpi n (TextNode ns) = foldl' (\l -> \r -> l ++ (dumpi n r)) "" ns

dumpResult (Right t) = dump t
dumpResult (Left e) = show e

numSubtitles (Left _) = 0
numSubtitles (Right s) = (length . sub) s

tests =
  describe "SrtParse" $ do
    it "sanity checks" $ do
      (True == False) `shouldBe` False

    it "Parses a timestamp" $ do
      runParser
        parseTimeStamp () "" "10:11:38,100" `shouldBe` (Right 36660100)

    it "Parses a plain char 1" $ do
      runParser plainChar () "" "abc" `shouldBe` (Right 'a')

    it "Parses a plain char 2" $ do
      runParser plainChar () "" "\nabc" `shouldBe` (Right '\n')

    it "Parses a plain char 3" $ do
      runParser plainChar () "" "\n\nabc" `shouldSatisfy` isLeft

    it "Parses plain 1" $ do
      runParser plain () "" "ABC" `shouldBe` Right (LeafText "ABC")

    it "Parses plain 2" $ do
      runParser plain () "" "plain followed by bold</b>" `shouldBe`
        (Right (LeafText "plain followed by bold"))

    it "Parses bold" $ do
      runParser pbold () "" "<b>ABC</b>" `shouldBe`
        Right (TextTag (TextNode [LeafText "ABC"]) Bold)

    it "Parses nested bold" $ do
      runParser pbold () "" "<b><b>ABC</b></b>" `shouldBe`
        Right (TextTag (TextNode [TextTag (TextNode [LeafText "ABC"]) Bold]) Bold)

    it "Parses a sequence of formatted and unformatted text" $ do
      runParser parseText () "" "<b>bold</b><i>italic</i>PLAIN<u>underline</u>plain"
        `shouldBe`
        Right (TextNode [
                  TextTag (TextNode [LeafText "bold"]) Bold,
                  TextTag (TextNode [LeafText "italic"]) Italic,
                  LeafText "PLAIN",
                  TextTag (TextNode [LeafText "underline"]) Underline,
                  LeafText "plain"]
              )

    it "Parses an entire subtitle entry" $ do
      runParser parseSubtitle () ""
        "128\n01:01:01,100 --> 01:02:02,000\nXXXX\nYYYYY\n\n" `shouldBe`
        Right (
          Subtitle {
             start = 3660100,
             stop = 3720000,
             subtext = [
               Text {
                  style_name = Nothing,
                  chunks = TextChunk {
                    format = Nothing,
                    text = "XXXX\nYYYYY"
                    }
                  }
               ]
             }
          )

    it "Parses an empty line" $ do
      runParser emptyLine () "" "\n\n" `shouldBe` Right ""

    it "Tries to parse an empty line, but fails" $ do
      runParser emptyLine () "" "\nx\n" `shouldSatisfy` (\x -> case x of
                                                          Left _ -> True
                                                          otherwise -> False)

    it "Parses multiple subtitles" $ do
      runParser parseFile () ""
        "128\n01:01:01,100 --> 01:02:02,000\nXXXX\n\n129\n01:01:01,100 --> 01:02:02,000\nXXXX\n\n" `shouldBe`
        Right (
          SubContainer {
             metadata = Metadata {desc = ""},
             styles = [],
             sub = [
               Subtitle {
                  start = 3660100,
                  stop = 3720000,
                  subtext = [Text {
                                style_name = Nothing,
                                chunks = TextChunk {format = Nothing, text = "XXXX"}}
                            ]},
               Subtitle {
                 start = 3660100,
                 stop = 3720000,
                 subtext = [Text {style_name = Nothing,
                                  chunks = TextChunk {format = Nothing, text = "XXXX"}
                                 }]}]})

    it "Parses multiple subtitles" $ do
      runParser parseFile () ""
        "128\n01:01:01,100 --> 01:02:02,000\nXXXX\n\n129\n01:01:01,100 --> 01:02:02,000\nXXXX" `shouldBe`
        Right (
          SubContainer {
             metadata = Metadata {desc = ""},
             styles = [],
             sub = [
               Subtitle {
                  start = 3660100,
                  stop = 3720000,
                  subtext = [Text {
                                style_name = Nothing,
                                chunks = TextChunk {format = Nothing, text = "XXXX"}}
                            ]},
               Subtitle {
                 start = 3660100,
                 stop = 3720000,
                 subtext = [Text {style_name = Nothing,
                                  chunks = TextChunk {format = Nothing, text = "XXXX"}
                                 }]}]})

    it "Reads a file and parses it" $
       do {
         s <- readFile "./samples/the.godfather.10.entries.srt";
         return $ numSubtitles (runParser parseFile () "" s);
       }
       `shouldReturn` 10
