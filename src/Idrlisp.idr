module Idrlisp

import CIO
import Data.SortedMap
import public Idrlisp.Value
import Idrlisp.Parser
import Idrlisp.Env
import Idrlisp.Syntax

%default covering

export
parseExpr : String -> Either String Value
parseExpr = Parser.parseToEnd

export
parseProgram : String -> Either String (List Value)
parseProgram = Parser.parseToEnd

export
newContext : List (String, Builtin) -> IO Context
newContext builtins =
  do
    env <- Env.new Nothing
    traverse_ (uncurry (install env)) Syntax.syntaxList
    pure $ MkContext env (fromList builtins)
  where
    install : Env Value -> String -> Syntax -> IO ()
    install env name syn = Env.define name (Pure (NSyntax syn)) env

export
runCompile : Context -> Compile a -> CIO String (a, Code Value)
runCompile ctx = run
  where
    run : Compile a -> CIO String (a, Code Value)
    run (Do inst) = pure ((), singleton inst)
    run (Refer sym) = do
      value <- lift $ Env.lookup sym (topLevel ctx)
      pure (eitherToMaybe value, empty)
    run (Block x) = do
      (_, code) <- run x
      pure (code, empty)
    run (Pure x) = pure (x, empty)
    run (Bind x f) = do
      (a, code) <- run x
      (b, code') <- run (f a)
      pure (b, code ++ code')
    run (Fail x) = throw x

export
runVM : Context -> VM a -> Cont -> CIO String (a, Cont)
runVM ctx = run
  where
    run : VM a -> Cont -> CIO String (a, Cont)
    run (Push value) cont = pure ((), record { stack $= (value ::) } cont)
    run Pop cont =
      case stack cont of
        x :: xs => pure (Just x, record { stack = xs } cont)
        _ => pure (Nothing, cont)
    run (Get name) cont = do
      value <- lift $ Env.lookup name (env cont)
      pure (value, cont)
    run (Define name value) cont = do
      r <- lift $ Env.define name value (env cont)
      pure (Right r, cont)
    run (Set name value) cont = do
      r <- lift $ Env.set name value (env cont)
      pure (r, cont)
    run CaptureEnv cont = pure (env cont, cont)
    run Next cont =
      case next (code cont) of
        Just (x, xs) => pure (Just x, record { code = xs } cont)
        Nothing => pure (Nothing, cont)
    run (Enter nenv ncode) (MkCont stack env code dump) = do
      nenv <- lift $ Env.new $ Just nenv
      if tailPosition code
         then pure ((), MkCont stack nenv ncode dump)
         else pure ((), MkCont stack nenv ncode ((env, code) :: dump))
    run Leave cont@(MkCont stack env code dump) =
      case dump of
        [] => pure (Left (), cont)
        (penv, pcode) :: rest => pure (Right (env, code), MkCont stack penv pcode rest)
    run DropCont cont = pure ((), MkCont [] (env cont) (singleton Leave) [])
    run (RestoreCont cont) _ = pure ((), cont)
    run CaptureCont cont = pure (cont, cont)
    run CaptureContext cont = pure (ctx, cont)
    run (LoadBuiltin name) cont = pure (lookup name (builtins ctx), cont)
    run (Action action) cont = pure (!(lift action), cont)
    run (Pure x) cont = pure (x, cont)
    run (Bind x f) cont = do
      (x, cont) <- run x cont
      run (f x) cont
    run (Fail err) cont = throw err

export
runMacroExpand : Context -> MacroExpand a -> CIO String a
runMacroExpand ctx = run
  where
    run : MacroExpand a -> CIO String a
    run (Refer sym) = do
      value <- lift $ Env.lookup sym (topLevel ctx)
      pure $ eitherToMaybe value
    run (Execute op) = fst <$> runVM ctx op (MkCont [] (topLevel ctx) empty [])
    run (Pure x) = pure x
    run (Bind x f) = do
      x <- run x
      run (f x)
    run (Fail err) = throw err

export
compileExpr : Context -> Value -> IO (Either String (Code Value))
compileExpr ctx expr = runCIO $ snd <$> runCompile ctx (load expr)

export
macroExpandExpr : Context -> Bool -> Value -> IO (Either String Value)
macroExpandExpr ctx recurse expr = runCIO $ runMacroExpand ctx (expand recurse expr)

export
execCode : Context -> Env Value -> Code Value -> IO (Either String Value)
execCode ctx env code = runCIO $ fst <$> runVM ctx instCycle (MkCont [] env code [])

export
evalExpr : Context -> Value -> IO (Either String Value)
evalExpr ctx expr = runCIO $ do
  expr <- runMacroExpand ctx (expand True expr)
  (_, code) <- runCompile ctx (load expr)
  (r, _) <- runVM ctx instCycle (MkCont [] (topLevel ctx) code [])
  pure r

export
evalProgram : Context -> List Value -> IO (Either (Nat, String) Value)
evalProgram ctx = go 0 Nil
  where
    go : Nat -> Value -> List Value -> IO (Either (Nat, String) Value)
    go index last [] = pure $ Right last
    go index last (x :: xs) =
      case !(evalExpr ctx x) of
        Left err => pure $ Left (index, err)
        Right last' => go (S index) last' xs

