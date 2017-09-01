module SrtPrinter where

import qualified SubContainer as SC
import Data.List
import Text.Printf

timeStampToStr :: Int -> String
timeStampToStr t =
  let ms = t `mod` 1000
      s = (t `div` 1000) `mod` 60
      m = ((t `div` (60 * 1000))) `mod` 60
      h = t `div` (60 * 60 * 1000) in
   printf "%02d:%02d:%02d,%03d" h m s ms

subtitleToStr :: SC.Subtitle -> String
subtitleToStr s =
  (timeStampToStr . SC.start $ s) ++
  "-->" ++
  (timeStampToStr . SC.stop $ s) ++
  "\n" ++
  (SC.text . SC.chunks . head . SC.subtext $ s)

subsToStr :: Int -> [SC.Subtitle] -> String
subsToStr _ [] = ""
subsToStr n (t:ts) =
  (show n) ++ "\n" ++
  subtitleToStr t ++ (if (null ts) then "" else "\n\n") ++
  subsToStr (n + 1) ts

subContainerToStr :: SC.SubContainer -> String
subContainerToStr = (subsToStr 1) . SC.sub
