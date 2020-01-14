module Main

import System
import Idrlisp
import TestRunner

abort : String -> IO a
abort msg = do
  fPutStrLn stderr msg
  exit 1

handleFileError : IO (Either FileError a) -> IO a
handleFileError x =
  case !x of
    Right r => pure r
    Left e => abort $ show e

main : IO ()
main =
  case !getArgs of
    [_, "-test", test] => do
      ctx <- newContext
      fh <- handleFileError $ openFile test Read
      r <- TestRunner.run ctx fh
      case r of
        Left e => abort e
        Right Z => pure ()
        Right n => abort $ "Total " ++ show n ++ " tests failed."
    args =>
      printLn args

