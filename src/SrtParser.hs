module SrtParser where

import qualified SubContainer as SC
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.Char
import Data.List

makeDefaultSubtitle :: Int -> Int -> String -> SC.Subtitle
makeDefaultSubtitle start stop text =
  SC.Subtitle start stop [SC.Text Nothing (SC.TextChunk Nothing text)]

c2i :: Char -> Int
c2i c = ord c - ord '0'

parseTimeStamp = do {
  h1 <- digit;
  h0 <- digit;
  char ':';
  m1 <- digit;
  m0 <- digit;
  char ':';
  s1 <- digit;
  s0 <- digit;
  char ',';
  ms2 <- digit;
  ms1 <- digit;
  ms0 <- digit;
  return ((((c2i h1) * 10 + (c2i h0)) * 3600 +
          ((c2i m1) * 10 + (c2i m0)) * 60) * 1000 +
          (c2i ms2) * 100 + (c2i ms1) * 10 + (c2i ms1));
  }


data SrtState = SrtState {
  bold :: Bool,
  italic :: Bool,
  underline :: Bool,
  color :: String
  } deriving (Eq, Show)

b s = SrtState True (italic s) (underline s) (color s)
i s = SrtState (bold s) True (underline s) (color s)
u s = SrtState (bold s) (italic s) True (color s)
c color s = SrtState (bold s) (italic s) (underline s) color

data TextTree = TextTag TextTree (SrtState -> SrtState)
              | LeafText String
              | TextNode [TextTree]

dump :: TextTree -> String
dump = dumpi 0
dumpi :: Int -> TextTree -> String
dumpi n (LeafText s) = replicate n ' ' ++  s ++ "\n"
dumpi n (TextTag t _) = replicate n ' ' ++ "t:\n" ++ dumpi (n + 1) t
dumpi n (TextNode ns) = foldl' (\l -> \r -> l ++ (dumpi n r)) "" ns

defaultState = SrtState False False False ""

boldOn = string "<b>" <|> string "{b}"
boldOff = string "</b>" <|> string "{/b}"

pbold = (\t -> TextTag t b) <$> (between (try boldOn) (try boldOff) parseText)

italicOn = string "<i>" <|> string "{i}"
italicOff = string "</i>" <|> string "{/i}"

pitalic = (\t -> TextTag t i) <$> (between (try italicOn) (try italicOff) parseText)

underlineOn = string "<u>" <|> string "{u}"
underlineOff = string "</u>" <|> string "{/u}"

punderline = (\t -> TextTag t u) <$>
             (between (try underlineOn) (try underlineOff) parseText)

colorOn = do {
  string "<font";
  spaces;
  string "color=\"";
  color <- manyTill anyChar (try (char '"'));
  string ">";
  return color;
  }

colorOff = string "</font>"
pcolor = do {
  cc <- try colorOn;
  t <- parseText;
  colorOff;
  return (TextTag t (c cc));
  }

emptyLine = do {
  endOfLine;
  endOfLine;
  return "";
  }

plain = (\t -> LeafText t) <$> many1 (noneOf "<>")

formatOrPlain = (try pbold) <|>
                (try pitalic) <|>
                (try punderline) <|>
                (try pcolor) <|>
                plain

parseText = (\t -> TextNode t) <$> many formatOrPlain

dumpResult (Right t) = dump t
dumpResult (Left e) = show e

test1 = runParser parseTimeStamp defaultState "" "10:11:39,100"
test2 = dumpResult $ runParser plain () "" "abcd abcsd <b>x</b> ahs\n\n"
test20 = dumpResult $ runParser plain () "" "abcd abcsd"
test21 = dumpResult $ runParser plain () "" "abcd abcsd  ahs\n\n"
test3 = runParser emptyLine () "" "\n\n"
test4 = dumpResult $ runParser pbold () "" "<b><i>abcd</i><b></b></b>\n\n"
test40 = dumpResult $ runParser pcolor () "" "<font color=\"ABC\">xxx </font>"
test6 = dumpResult $ runParser parseText () ""
        "Hello <b>my name is</b>\
        \What is yours? <font color=\"XXX\">\
        \My color is </font>\n\
        \\n"

-- test2 = runParser boldOn defaultState "" "b{b}"
-- test3 = runParser boldOn defaultState "" "{b}"
-- test4 = runParser boldOff defaultState "" "{b}"
test5 = let r = runParser parseText () ""
                "xxx"
        in case r of
            Right t -> dump t
            Left e -> "Error"
