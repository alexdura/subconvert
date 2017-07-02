module SrtParser where

import qualified SubContainer as SC
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (modifyState)
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

defaultState = SrtState False False False ""

boldOn = do {
  string "<b>" <|> string "{b}";
  modifyState (\s -> SrtState True (italic s) (underline s) (color s));
  }

boldOff = do {
  string "</b>" <|> string "{/b}";
  modifyState (\s -> SrtState False (italic s) (underline s) (color s));
  }

italicOn = do {
  string "<i>" <|> string "{i}";
  modifyState (\s -> SrtState (bold s) True (underline s) (color s));
  }

italicOff = do {
  string "</i>" <|> string "{/i}";
  modifyState (\s -> SrtState (bold s) False (underline s) (color s));
  }

underlineOn = do {
  string "<u>" <|> string "{u}";
  modifyState (\s -> SrtState (bold s) (italic s) True (color s));
  }

underlineOff = string "</u>" <|> string "{/u}"

parseColorOn = do {
  string "<font";
  spaces;
  string "color=\"";
  color <- noneOf "\"";
  string "\">";
  modifyState (\s -> SrtState (bold s) (italic s) (underline s) color);
  }

parseColorOff = do {
  string "</color>";
  modifyState (\s -> SrtState (bold s) (italic s) (underline s) "");
  }

parseText = do {

  }


test1 = runParser parseTimeStamp defaultState "" "10:11:39,100"
test2 = runParser boldOn defaultState "" "b{b}"
test3 = runParser boldOn defaultState "" "{b}"
test4 = runParser boldOff defaultState "" "{b}"
