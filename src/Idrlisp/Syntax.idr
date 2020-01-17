module Idrlisp.Syntax

import Idrlisp.Value

%default covering

export
def : String -> Value -> Compile ()
def sym x = do
  load x
  Do $ Def sym
  Do $ Ldc Nil

export
set : String -> Value -> Compile ()
set sym x = do
  load x
  Do $ Set sym
  Do $ Ldc Nil

export
begin : List Value -> Compile ()
begin [] = Do $ Ldc Nil
begin [x] = load x
begin (x :: xs) = do
  load x
  Do Pop
  begin xs

export
if' : Value -> Value -> Value -> Compile ()
if' c t e = do
  load c
  t <- Block $ load t *> Do Leave
  e <- Block $ load e *> Do Leave
  Do $ Sel t e

export
fun : Pattern -> List Value -> Compile ()
fun pattern body = do
  body <- Block $ begin body *> Do Leave
  Do $ Ldf pattern body

export
macro : Pattern -> List Value -> Compile ()
macro pattern body = do
  body <- Block $ begin body
  Do $ Ldm pattern body

export
builtin : String -> Compile ()
builtin name = Do $ Ldb name

export
quote : Value -> Compile ()
quote expr = Do $ Ldc expr

mkSyntax : String -> (msig : List Bool) -> (sig : ValueArgsSignature) -> (SignatureType sig -> Compile ()) -> (String, Syntax)
mkSyntax name msig sig handle = (name, MkSyntax macroExpand compile)
  where
    macroExpand : List Value -> MacroExpand (List Value)
    macroExpand args = go msig args
      where
        go : List Bool -> List Value -> MacroExpand (List Value)
        go (s :: ss) (x :: xs) = [| ifThenElse s (expand True) pure x :: go ss xs |]
        go _ xs = pure xs

    compile : List Value -> Compile ()
    compile args =
      case match sig args of
        Nothing => Fail $ "Compile error: expected " ++ show (Sym name :: sig)
        Just x => handle x

export
syntaxList : List (String, Syntax)
syntaxList =
  [ mkSyntax "def" [False, True] [Sym "sym", Any "x"] $ uncurry def
  , mkSyntax "set!" [False, True] [Sym "sym", Any "x"] $ uncurry set
  , mkSyntax "begin" [] (Rest (Any "body")) begin
  , mkSyntax "if" [True, True, True] [Any "cond", Any "then", Any "else"] $ \(c, t, e) => if' c t e
  , mkSyntax "fun" [False] (Pat "pattern" :: Rest (Any "body")) $ uncurry fun
  , mkSyntax "macro" [False] (Pat "pattern" :: Rest (Any "body")) $ uncurry macro
  , mkSyntax "builtin" [False] [Sym "sym"] builtin
  , mkSyntax "quote" [False] [Any "expr"] quote
  ]

