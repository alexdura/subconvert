module SubParser where

import qualified SubContainer as SC
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec (Parsec)
import Data.Char
import Data.List
import Common

parseSubFile = undefined

control :: Parsec String () a -> Parsec String () a
control = between (char '{') (char '}')


number :: Parsec String () Int
number = (foldl' (\acc -> \c -> 10 * acc + (c2i c)) 0) <$> (many1 digit)

data CtrlCode = Bold
              | Italic
              | Underline
              | Color String
              | Pos Int Int
              | Font String
              | CharSet String
              | NewLine
              deriving (Eq, Show)

data SubEntry = Ctrl {ctrl :: CtrlCode}
              | SubText {text :: String}
              deriving (Eq, Show)

data Sub = Sub {
  startFrame :: Int,
  endFrame :: Int,
  subs :: [SubEntry]
  } deriving (Eq, Show)

makeDefault :: [SubEntry] -> Sub
makeDefault s = Sub (-1) (-1) s

isDefault :: Sub -> Bool
isDefault s = startFrame s < 0


parseSubtitle :: Parsec String () Sub
parseSubtitle = do {
  startFrame <- control number;
  endFrame <- control number;
  skipMany (control (many1 $ noneOf "}"));
  line <- manyTill anyChar (endOfLine <|> ((\_ -> '\n') <$> eof));
  return $ Sub startFrame endFrame [SubText (replace '|' '\n' line)];
}

parseDefault :: Parsec String () Sub
parseDefault = do {
  control $ string "DEFAULT:";
  manyTill (control (many1 $ noneOf "}")) endOfLine;
  return $ makeDefault [];
  }

parseFileInternal :: Parsec String () [Sub]
parseFileInternal = many ((try parseSubtitle) <|> parseDefault)

parseFile :: Int -> Parsec String () SC.SubContainer
parseFile mspf = do {
  subtitles <- parseFileInternal;
  let f = filter (\s -> not (isDefault s))
      m = map (\s -> makeDefaultSubtitle (mspf * (startFrame s)) (mspf * (endFrame s))
                    (text $ head (subs s))) in
   return (SC.SubContainer {
              SC.metadata = SC.Metadata "",
              SC.styles = [],
              SC.sub = (m . f) subtitles
                       })
  }

dummy = runParser (control $ char '.') () "" "{.}"
