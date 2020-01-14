module Idrlisp.Value

import public Idrlisp.Sexp
import public Idrlisp.Pattern
import public Idrlisp.Signature
import public Idrlisp.Code
import Idrlisp.Env

%default covering

mutual
  public export
  Value : Type
  Value = Sexp Native

  public export
  ValueList : Type
  ValueList = SList Native

  public export
  data Native : Type where
    NBuiltin : Builtin -> Native
    NSyntax : Syntax -> Native
    NFun : Closure -> Native
    NMacro : Closure -> Native

  public export
  record Builtin where
    constructor MkBuiltin
    run : List Value -> Run ()

  public export
  record Syntax where
    constructor MkSyntax
    macroExpand : List Value -> MacroExpand (List Value)
    compile : List Value -> Compile ()

  public export
  record Closure where
    constructor MkClosure
    env : Env Value
    pattern : Pattern
    body : Code Value

  namespace Run
    public export
    data Run : Type -> Type where
      -- TODO

      Pure : a -> Run a
      Bind : Run a -> (a -> Run b) -> Run b
      Fail : String -> Run a

  namespace Macro
    public export
    data MacroExpand : Type -> Type where
      -- TODO

      Pure : a -> MacroExpand a
      Bind : MacroExpand a -> (a -> MacroExpand b) -> MacroExpand b
      Fail : String -> MacroExpand a

  namespace Compile
    public export
    data Compile : Type -> Type where
      Do : Inst Value -> Compile ()
      Refer : String -> Compile (Maybe Value)
      Block : Compile () -> Compile (Code Value)

      Pure : a -> Compile a
      Bind : Compile a -> (a -> Compile b) -> Compile b
      Fail : String -> Compile a

export
Show Native where
  show (NBuiltin x) = "<builtin>"
  show (NSyntax x) = "<syntax>"
  show (NFun x) = "<fun>"
  show (NMacro x) = "<macro>"

namespace Signature
  public export
  data NativeSignature
    = NBuiltin
    | NSyntax
    | NFun
    | NMacro

  public export
  Match NativeSignature Native where
    SignatureType NBuiltin = Builtin
    SignatureType NSyntax = Syntax
    SignatureType NFun = Closure
    SignatureType NMacro = Closure

    match NBuiltin (NBuiltin x) = Just x
    match NBuiltin _ = Nothing
    match NSyntax (NSyntax x) = Just x
    match NSyntax _ = Nothing
    match NFun (NFun x) = Just x
    match NFun _ = Nothing
    match NMacro (NMacro x) = Just x
    match NMacro _ = Nothing

  public export
  ValueSignature : Type
  ValueSignature = Signature NativeSignature

  public export
  ValueArgsSignature : Type
  ValueArgsSignature = ArgsSignature NativeSignature

  public export
  Builtin : String -> ValueSignature
  Builtin s = Pure s NBuiltin

  public export
  Syntax : String -> ValueSignature
  Syntax s = Pure s NSyntax

  public export
  Fun : String -> ValueSignature
  Fun s = Pure s NFun

  public export
  Macro : String -> ValueSignature
  Macro s = Pure s NMacro

-- NOTE: Can we extract namespace Compile as another module?
-- How should we abstract the representation of Value and record Syntax?
namespace Compile
  export
  Functor Compile where
    map f a = Bind a (\a' => Pure (f a'))

  export
  Applicative Compile where
    pure = Pure
    (<*>) f a = Bind f (\f' => Bind a (\a' => Pure (f' a')))

  export
  Monad Compile where
    (>>=) = Bind

  export
  refer : Value -> Compile (Maybe Value)
  refer (Sym sym) = Refer sym
  refer _ = pure Nothing

  mutual
    export
    eval : Value -> Compile ()
    eval (Sym sym) = Do $ Ldv sym
    eval (f :: xs) with (cast {to = ValueList} xs)
      | Proper args =
          case !(refer f) of
            Just (Pure (NSyntax syn)) => compile syn args
            _ => apply f args
      | Improper _ = Fail $ "Improper list: " ++ show (f :: xs)
    eval x = Do $ Ldc x

    export
    apply : Value -> List Value -> Compile ()
    apply f args = do
      eval f
      traverse_ eval args
      Do $ App (length args)

