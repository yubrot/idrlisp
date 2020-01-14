module Idrlisp.Syntax

import Idrlisp.Value

%default covering

export
def : String -> Value -> Compile ()
def sym x = do
  eval x
  Do $ Def sym
  Do $ Ldc Nil

export
set : String -> Value -> Compile ()
set sym x = do
  eval x
  Do $ Set sym
  Do $ Ldc Nil

export
begin : List Value -> Compile ()
begin [] = Do $ Ldc Nil
begin [x] = eval x
begin (x :: xs) = do
  eval x
  Do Pop
  begin xs

export
if' : Value -> Value -> Value -> Compile ()
if' c t e = do
  eval c
  t <- Block $ eval t *> Do Leave
  e <- Block $ eval e *> Do Leave
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

mkSyntax : String -> (sig : ValueArgsSignature) -> (SignatureType sig -> Compile ()) -> (String, Syntax)
mkSyntax name sig handle = (name, MkSyntax macroExpand compile)
  where
    macroExpand : List Value -> MacroExpand (List Value)
    macroExpand = Pure

    compile : List Value -> Compile ()
    compile args =
      case match sig args of
        Nothing => Fail $ "expected " ++ show (Sym name :: sig)
        Just x => handle x

export
syntaxList : List (String, Syntax)
syntaxList =
  [ mkSyntax "def" [Sym "sym", Any "x"] $ uncurry def
  , mkSyntax "set!" [Sym "sym", Any "x"] $ uncurry set
  , mkSyntax "begin" (Rest (Any "body")) begin
  , mkSyntax "if" [Any "cond", Any "then", Any "else"] $ \(c, t, e) => if' c t e
  , mkSyntax "fun" (Pat "pattern" :: Rest (Any "body")) $ uncurry fun
  , mkSyntax "macro" (Pat "pattern" :: Rest (Any "body")) $ uncurry macro
  , mkSyntax "builtin" [Sym "sym"] builtin
  , mkSyntax "quote" [Any "expr"] quote
  ]

