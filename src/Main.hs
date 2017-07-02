{-# LANGUAGE PatternGuards #-}

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment

import Debug.Trace (trace)

data OptionFlags = SRTFormat
                 | SubFormat
                 deriving (Eq, Show)

options = [
  Option [] ["srt"] (NoArg SRTFormat) "SRT format",
  Option [] ["sub"] (NoArg SubFormat) "Sub format"
  ]

main :: IO ()
main = putStrLn "The subtitle converter"
