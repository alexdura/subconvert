module SubParserSpec (tests) where

import Test.Hspec
import SubParser
import SubContainer
import Data.List
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import Text.Parsec.Error

import Common

numSubtitles (Left _) = 0
numSubtitles (Right s) = (length . sub) s

tests =
  describe "SubParse" $ do
    -- it "Dummy run" $ do
    --   runParser parseSubFile () "" "" `shouldSatisfy` (not . isLeft)

    it "Parses a control sequence" $ do
      runParser (control $ string "ABCD") () "" "{ABCD}" `shouldBe` Right "ABCD"

    it "Parses a number in base 10" $ do
      runParser number () "" "393" `shouldBe` Right 393

    it "Parses another number in base 10, with leading zeros" $ do
      runParser number () "" "00012" `shouldBe` Right 12

    it "Parses an entire subtitle line" $ do
      runParser parseSubtitle () "" "{199}{201}Hello!\n" `shouldBe`
        Right (Sub {
                  startFrame = 199,
                  endFrame = 201,
                  subs = [SubText "Hello!"]})

    it "Parses an entire subtitle line and ignores formatting" $ do
      runParser parseSubtitle () "" "{199}{201}{to be ignored}Hello!\n" `shouldBe`
        Right (Sub {
                  startFrame = 199,
                  endFrame = 201,
                  subs = [SubText "Hello!"]})

    it "Parses an entire subtitle line and ignores formatting" $ do
      runParser parseSubtitle () "" "{199}{201}{to be ignored}Hello!|Hello again!\n"
        `shouldBe`
        Right (Sub {
                  startFrame = 199,
                  endFrame = 201,
                  subs = [SubText "Hello!\nHello again!"]})

    it "Parses an default line and ignores formatting" $ do
      runParser parseDefault () "" "{DEFAULT:}{199}{201}{to be ignored}\n"
        `shouldBe`
        Right (Sub {
                  startFrame = -1,
                  endFrame = -1,
                  subs = []})


    it "Parses multiple .sub lines, including a DEFAULT" $ do
      runParser (parseFile 1) () "" "{1}{2}Hello!\n{DEFAULT:}{x}{y}{bla}\n{3}{4}Hello 2!"
        `shouldBe`
            Right (SubContainer {
                      metadata = Metadata {desc = ""},
                      styles = [],
                      sub = [Subtitle {
                                start = 1, stop = 2,
                                subtext = [Text {
                                              style_name = Nothing,
                                              chunks = TextChunk {
                                                format = Nothing,
                                                SubContainer.text = "Hello!"}}]},
                             Subtitle {
                               start = 3,
                               stop = 4,
                               subtext = [Text {style_name = Nothing,
                                                chunks = TextChunk {
                                                  format = Nothing,
                                                  SubContainer.text = "Hello 2!"}}]}
                            ]})

    it "Reads a file and parses it" $
       do {
         s <- readFile "./samples/the.godfather.sub";
         return $ numSubtitles (runParser (parseFile 1) () "" s);
       }
       `shouldReturn` 702
