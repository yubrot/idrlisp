module TestRunner

import Idrlisp
import CIO

%default covering

data Command
  = ParseSuccess String String
  | ParseFailure String
  | CompileSuccess String String
  | CompileFailure String
  | EvalSuccess String String
  | EvalFailure String
  | EvalAll String

record TestCase where
  constructor MkTestCase
  header : String
  command : Command

namespace Exec
  parses : String -> CIO String Value
  parses input = either throw pure $ parse input

  compiles : Context -> Value -> CIO String (Code Value)
  compiles ctx value = lift (compileOnContext ctx value) >>= either throw pure

  success : Show a => String -> a -> CIO String ()
  success expected a =
    if trim (show a) == expected
       then pure ()
       else throw $ show a

  failure : Show b => (a -> CIO String b) -> a -> CIO String ()
  failure f x = do
    err <- (Just <$> f x) `catch` \_ => pure Nothing
    case err of
      Nothing => pure ()
      Just x => throw $ show x

  execCommand : Context -> Command -> CIO String ()
  execCommand ctx (ParseSuccess input output) = parses input >>= success output
  execCommand ctx (ParseFailure input) = failure parses input
  execCommand ctx (CompileSuccess input output) = parses input >>= compiles ctx >>= success output
  execCommand ctx (CompileFailure input) = parses input >>= failure (compiles ctx)
  execCommand ctx (EvalSuccess input output) = pure ()
  execCommand ctx (EvalFailure input) = pure ()
  execCommand ctx (EvalAll input) = pure ()

  execTestCase : Context -> TestCase -> CIO String Bool
  execTestCase ctx (MkTestCase header command) =
    do
      execCommand ctx command
      pure False
    `catch`
    \e => do
      lift $ fPutStrLn stderr $ "Test failed at " ++ header ++ ": " ++ e
      pure True

namespace Read
  readLine : File -> CIO String String
  readLine fh = do
    a <- lift $ fGetLine fh
    case a of
      Right s =>
        case length s of
          Z => throw "End of input"
          S k => pure $ substr 0 k s
      Left e => throw $ show e

  readLines : Nat -> File -> CIO String String
  readLines k fh = do
    strs <- sequence $ replicate k $ readLine fh
    pure $ concat $ intersperse "\n" strs

  readCommand : File -> CIO String Command
  readCommand fh =
    case split (== ' ') !(readLine fh) of
      ["PARSE_SUCCESS", input, output] => do
        input <- readLines (cast input) fh
        output <- readLines (cast output) fh
        pure $ ParseSuccess input output
      ["PARSE_FAILURE", input] => do
        input <- readLines (cast input) fh
        pure $ ParseFailure input
      ["COMPILE_SUCCESS", input, output] => do
        input <- readLines (cast input) fh
        output <- readLines (cast output) fh
        pure $ CompileSuccess input output
      ["COMPILE_FAILURE", input] => do
        input <- readLines (cast input) fh
        pure $ CompileFailure input
      ["EVAL_SUCCESS", input, output] => do
        input <- readLines (cast input) fh
        output <- readLines (cast output) fh
        pure $ EvalSuccess input output
      ["EVAL_FAILURE", input] => do
        input <- readLines (cast input) fh
        pure $ EvalFailure input
      ["EVAL_ALL", input] => do
        input <- readLines (cast input) fh
        pure $ EvalAll input
      xs =>
        throw $ "Unknown test comamnd: " ++ show xs

  readTestCases : File -> CIO String (List TestCase)
  readTestCases fh = do
    header <- (Just <$> readLine fh) `catch` \_ => pure Nothing
    case header of
      Nothing => pure []
      Just header => do
        command <- readCommand fh
        testCases <- readTestCases fh
        pure $ MkTestCase header command :: testCases

export
run : Context -> File -> IO (Either String Nat)
run ctx fh = runCIO $ do
  testCases <- readTestCases fh
  results <- traverse (execTestCase ctx) testCases
  pure $ length $ filter id results

