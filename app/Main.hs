module Main where
import System.IO
import System.Environment
import qualified InputOutput
import qualified NaiveIteration
import qualified MatMul
import qualified MatMulUnboxed
import qualified PolyMul
import qualified PolyMulUnboxed

main = do
  args <- getArgs
  progName <- getProgName
  case args of
    "InputOutput":_ -> InputOutput.main
    "List":_ -> NaiveIteration.printAsList
    "NaiveIteration":_ -> NaiveIteration.main
    "MatMul":_ -> MatMul.main
    "MatMulUnboxed":_ -> MatMulUnboxed.main
    "PolyMul":_ -> PolyMul.main
    "PolyMulUnboxed":_ -> PolyMulUnboxed.main
    _ -> hPutStrLn stderr $ progName ++ " InputOutput|List|NaiveIteration|MatMul|MatMulUnboxed|PolyMul|PolyMulUnboxed"
