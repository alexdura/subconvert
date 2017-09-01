{-# LANGUAGE PatternGuards #-}

import Control.Monad
import Data.Maybe
import Debug.Trace (trace)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data OptionFlags = FPS String
                 | Output String
                 | Input String
                 | Version
                 deriving (Eq, Show)

compilerOpts :: [String] -> IO ([OptionFlags], [String])
compilerOpts argv =
  case getOpt Permute options argv of
   (o,n,[]  ) -> return (o,n)
   (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: subconvert [OPTION...] files..."

options =[
  Option ['f'] ["fps"] (ReqArg FPS  "FPS") "FPS",
  Option ['o'] ["output"] (ReqArg Output "FILE") "Output file",
  Option [] [] (ReqArg Input "FILE") "Input file",
  Option ['v'] ["--version"] (NoArg Version) "Version"
  ]

data InputArgs = InputArgs {
  fps :: Float,
  output :: String,
  input :: String,
  version :: Bool
  }

hFlag :: OptionFlags -> InputArgs -> IO(InputArgs)
-- hFlag (FPS s) args = do {
--   f <- readIO s;
--   return (args {fps = f});
--   }

hFlag (FPS s) args = (readIO s) >>= (\f -> return (args {fps = f}))
hFlag (Output s) args = return (args {output = s})
hFlag Version args = return (args {version = True})

defaultArgs = InputArgs {
  fps = 30,
  output = "out.srt",
  input = "",
  version = False
}

collectArgs :: ([OptionFlags], [String]) -> IO(InputArgs)
collectArgs (flags, positional) = do {
  args <- foldM (flip  hFlag) defaultArgs flags;
  putStrLn ("FPS: " ++ ((show . fps) args));
  putStrLn ("Input: " ++ ((show . input) args));
  putStrLn ("Output: " ++ ((show . output) args));
  putStrLn ("Version: " ++ ((show . version) args));
  case (length positional) of
   0 -> ioError $ userError "No input file specified"
   1 -> return (args {input = head positional})
   otherwise -> ioError $ userError "Too many input files"
  }

main :: IO ()
main = do {
  putStrLn "The subtitle converter";
  argv <- getArgs;
  opts <- compilerOpts argv;
  collectArgs opts;
  return ();
  }
