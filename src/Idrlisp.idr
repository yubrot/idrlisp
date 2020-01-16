module Idrlisp

import CIO
import public Idrlisp.Value
import Idrlisp.Parser as Parser
import Idrlisp.Env as Env
import Idrlisp.Syntax as Syntax

%default covering

export
parse : String -> Either String Value
parse = Parser.parseToEnd

export
record Context where
  constructor MkContext
  topLevel : Env Value

export
newContext : IO Context
newContext =
  do
    env <- Env.new Nothing
    install syntaxList env
    pure $ MkContext env
  where
    install : List (String, Syntax) -> Env Value -> IO ()
    install [] env = pure ()
    install ((name, syn) :: xs) env = do
      Env.define name (Pure (NSyntax syn)) env
      install xs env

export
compileOnContext : Context -> Value -> IO (Either String (Code Value))
compileOnContext ctx v = runCIO $ snd <$> execCompiler (eval v)
  where
    execCompiler : Compile a -> CIO String (a, Code Value)
    execCompiler (Do inst) = pure ((), singleton inst)
    execCompiler (Refer sym) = do
      value <- lift $ Env.lookup sym (topLevel ctx)
      pure (eitherToMaybe value, empty)
    execCompiler (Block x) = do
      (_, code) <- execCompiler x
      pure (code, empty)
    execCompiler (Pure x) = pure (x, empty)
    execCompiler (Bind x f) = do
      (a, code) <- execCompiler x
      (b, code') <- execCompiler (f a)
      pure (b, code ++ code')
    execCompiler (Fail x) = throw x

