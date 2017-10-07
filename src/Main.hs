{-# LANGUAGE PatternGuards #-}

import Control.Monad
import Data.Maybe
import Debug.Trace (trace)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import qualified SrtParser as SrtP
import qualified SubParser as SubP
import qualified SrtPrinter as SrtW
import Text.Parsec (runParser)

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
  } deriving(Show)

hFlag :: OptionFlags -> InputArgs -> IO(InputArgs)
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
  case (length positional) of
   0 -> ioError $ userError "No input file specified"
   1 -> return (args {input = head positional})
   otherwise -> ioError $ userError "Too many input files"
  }

-- convert frame/s to ms/frame
frameRate :: Float -> Int
frameRate f = round ((1.0 / f) * 1000)


execute :: InputArgs -> IO ()
execute s = do {
  input <- readFile $ input s;
  case runParser (SubP.parseFile (frameRate $ fps s)) () "" input of
   Left err -> ioError $ userError "Parsing error"
   Right tsubs ->
     writeFile (output s) (SrtW.subContainerToStr tsubs);
  }

main :: IO ()
main = do {
  putStrLn "The subtitle converter";
  argv <- getArgs;
  opts <- compilerOpts argv;
  s <- collectArgs opts;
  putStrLn $ show s;
  execute s;
  }
