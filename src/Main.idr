module Main

import System
import CIO
import Idrlisp
import Idrlib
import TestRunner

liftFileIO : IO (Either FileError a) -> CIO String a
liftFileIO x =
  case !(lift x) of
    Right r => pure r
    Left e => throw $ show e

parseAndEvalProgram : Context -> String -> String -> CIO String Value
parseAndEvalProgram ctx name input = do
  let label = name ++ ": "
  program <-
    case parseProgram input of
      Right program => pure program
      Left err => throw $ name ++ ": " ++ err
  result <- lift $ evalProgram ctx program
  case result of
    Right value => pure value
    Left (index, err) => throw $ name ++ "[" ++ show index ++ "]: " ++ err

parseAndEvalFiles : Context -> List String -> CIO String ()
parseAndEvalFiles ctx [] = pure ()
parseAndEvalFiles ctx (file :: files) = do
  input <- liftFileIO $ readFile file
  _ <- parseAndEvalProgram ctx file input
  parseAndEvalFiles ctx files

runRepl : Context -> CIO String ()
runRepl ctx =
  do
    lift $ fPutStrLn stderr "[idrlisp REPL]"
    loop repl
  where
    loop : Monad m => m Bool -> m ()
    loop m = if !m then pure () else loop m

    repl : CIO String Bool
    repl = do
      lift $ fPutStr stderr "> "
      lift $ fflush stderr
      input <- liftFileIO $ fGetLine stdin
      if input == "" then
        pure True
      else do
        result <- (Right <$> parseAndEvalProgram ctx "<stdin>" input) `catch` (pure . Left)
        case result of
          Right r => lift $ putStrLn $ show r
          Left err => ignore $ lift $ fPutStrLn stderr err
        pure False

initContext : List String -> Bool -> CIO String Context
initContext args boot = do
  -- TODO: handle boot
  builtins <- lift $ initIdrlib args
  lift $ newContext builtins

main : IO ()
main =
  case !(runCIO $ handleArgs !getArgs) of
    Right () => pure ()
    Left err => do
      fPutStrLn stderr err
      exit 1
  where
    splitArgs : List String -> (List String, List String)
    splitArgs args =
      case splitOn "--" args of
        xs :: xss => (xs, concat $ intersperse ["--"] xss)
        [] => ([], [])

    handleArgs : List String -> CIO String ()
    handleArgs [_] = do
      ctx <- initContext [] True
      runRepl ctx
    handleArgs [_, "-test", test] = do
      ctx <- initContext [] False
      fh <- liftFileIO $ openFile test Read
      testResult <- lift $ TestRunner.run ctx fh
      case testResult of
        Left err => throw err
        Right Z => pure ()
        Right fails => throw $ "Total " ++ show fails ++ " tests failed."
    handleArgs (_ :: args) = do
      let (files, lispArgs) = splitArgs args
      ctx <- initContext lispArgs True
      parseAndEvalFiles ctx files
    handleArgs _ = pure ()

