module SrtParser where

import qualified SubContainer as SC
import Text.ParserCombinators.Parsec
--import Text.Parsec.Prim (modifyState)
import Text.Parsec.Char
import Data.Char

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

defaultState = SrtState False False False ""

boldOn = try (string "<b>") <|> string "{b}"
boldOff = try (string "</b>") <|> string "{/b}"

pbold = (\t -> TextTag t b) <$> (between boldOn boldOff parseText)

italicOn = try (string "<i>") <|> string "{i}"
italicOff = try (string "</i>") <|> string "{/i}"

pitalic = (\t -> TextTag t i) <$> (between italicOn italicOff parseText)

underlineOn = try (string "<u>") <|> string "{u}"
underlineOff = try (string "</u>") <|> string "{/u}"

punderline = (\t -> TextTag t u) <$> (between underlineOn underlineOff parseText)

colorOn = do {
  string "<font";
  spaces;
  string "color=\"";
  color <- manyTill anyChar (try (char '"'));
  string "\">";
  return color;
  }

colorOff = string "</color>"
pcolor = do {
  cc <- colorOn;
  t <- parseText;
  colorOff;
  return (TextTag t (c cc));
  }

emptyLine = do {
  endOfLine;
  spaces;
  endOfLine;
  return "";
  }

plain = do {
  t <- manyTill anyChar (try emptyLine <|>
                         try boldOn <|>
                         try italicOn <|>
                         try colorOn <|>
                         try underlineOn <|>
                         try colorOn);
  return (LeafText t);
  }

formatOrPlain = (try pbold) <|>
                (try pitalic) <|>
                (try punderline) <|>
                (try pcolor) <|>
                plain

parseText = (\t -> TextNode t) <$> many1 formatOrPlain

test1 = runParser parseTimeStamp defaultState "" "10:11:39,100"
-- test2 = runParser boldOn defaultState "" "b{b}"
-- test3 = runParser boldOn defaultState "" "{b}"
-- test4 = runParser boldOff defaultState "" "{b}"
test5 = runParser parseText () ""
        "<b> HEllo <i> JKJKJK </i> xxx </b>"
