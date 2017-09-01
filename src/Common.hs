module Common where

import Data.Char
import qualified SubContainer as SC

c2i :: Char -> Int
c2i c = ord c - ord '0'

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

makeDefaultSubtitle :: Int -> Int -> String -> SC.Subtitle
makeDefaultSubtitle start stop text =
  SC.Subtitle start stop [SC.Text Nothing (SC.TextChunk Nothing text)]

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\e -> if e == x then y else e)
