{-# LANGUAGE PatternGuards #-}

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment

import Debug.Trace (trace)

main :: IO ()
main = putStrLn "The subtitle converter"
