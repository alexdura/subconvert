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
control p = (char '{') *> p <* (char '}')

number :: Parsec String () Int
number = (foldl' (\acc -> \c -> 10 * acc + (c2i c)) 0) <$> (many1 digit)


-- parseSubtitle = do {

--  }

dummy = runParser (control $ char '.') () "" "{.}"
