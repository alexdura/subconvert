module SrtParser where

import qualified SubContainer as SC
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec (Parsec)
import Data.Char
import Data.List
import Common

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

data Tag = Bold
         | Italic
         | Underline
         | Color String
         deriving (Eq, Show)

data TextTree = TextTag TextTree Tag
              | LeafText String
              | TextNode [TextTree]
              deriving (Eq, Show)

linearize :: TextTree -> String

linearize (TextTag tree tag) =
  case tag of
   Bold -> "<b>" ++ (linearize tree) ++ "</b>"
   Italic -> "<i>" ++ (linearize tree) ++ "</i>"
   Underline -> "<u>" ++ (linearize tree) ++ "</u>"
   Color c -> "<font color=\"" ++ c ++ ">" ++ (linearize tree) ++ "</font>"

linearize (LeafText s) = s

linearize (TextNode ts) = foldl' (\l -> \r -> l ++ (linearize r)) "" ts

boldOn = string "<b>" <|> string "{b}"
boldOff = string "</b>" <|> string "{/b}"

pbold = (\t -> TextTag t Bold) <$> (between (try boldOn) (try boldOff) parseText)

italicOn = string "<i>" <|> string "{i}"
italicOff = string "</i>" <|> string "{/i}"

pitalic = (\t -> TextTag t Italic) <$> (between (try italicOn) (try italicOff) parseText)

underlineOn = string "<u>" <|> string "{u}"
underlineOff = string "</u>" <|> string "{/u}"

punderline = (\t -> TextTag t Underline) <$>
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
  return (TextTag t (Color cc));
  }

emptyLine = do {
  endOfLine;
  endOfLine;
  return "";
  }

plainChar = do {
  c <- lookAhead anyChar;
  case c of
   '<' -> unexpected "<"
   '\n' -> do {
     anyChar;
     notFollowedBy (char '\n');
     return c;
     }
   otherwise -> anyChar;
  }

plain = (\t -> LeafText t) <$> (many1 (try plainChar))

formatOrPlain = (try pbold) <|>
                (try pitalic) <|>
                (try punderline) <|>
                (try pcolor) <|>
                plain

parseText :: Parsec String () TextTree
parseText = (\t -> TextNode t) <$> many formatOrPlain

dummy1 = runParser emptyLine () "" "\n\n"
dummy2 = runParser parseTimeStamp () "" "10:11:39,100"

parseSubtitle = do {
  -- ignore the sequential index
  skipMany1 digit;
  endOfLine;
  startTime <- parseTimeStamp;
  spaces;
  string "-->";
  spaces;
  endTime <- parseTimeStamp;
  endOfLine;
  text <- parseText;
  return (makeDefaultSubtitle startTime endTime (linearize text));
  }

parseFile = do {
  subs <- sepEndBy parseSubtitle (try emptyLine);
  return (SC.SubContainer {
             SC.metadata = SC.Metadata "",
             SC.styles = [],
             SC.sub = subs
                   })

  }
