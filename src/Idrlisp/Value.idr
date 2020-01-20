module Idrlisp.Value

import Data.SortedMap
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
    run : List Value -> VM ()

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

  namespace VM
    public export
    data VM : Type -> Type where
      -- S
      Push : Value -> VM ()
      Pop : VM (Maybe Value)

      -- E
      Get : String -> VM (Either String Value)
      Define : String -> Value -> VM (Either String ())
      Set : String -> Value -> VM (Either String ())
      CaptureEnv : VM (Env Value)

      -- C
      Next : VM (Maybe (Inst Value))

      -- D
      Enter : Env Value -> Code Value -> VM ()
      Leave : VM (Either () (Env Value, Code Value))

      -- for continuation support
      DropCont : VM ()
      RestoreCont : Cont -> VM ()
      CaptureCont : VM Cont

      -- for eval/macroexpand support
      CaptureContext : VM Context

      LoadBuiltin : String -> VM (Maybe Builtin)
      Action : IO a -> VM a

      Pure : a -> VM a
      Bind : VM a -> (a -> VM b) -> VM b
      Fail : String -> VM a

    public export
    record Cont where
      constructor MkCont
      stack : List Value
      env : Env Value
      code : Code Value
      dump : List (Env Value, Code Value)

  namespace MacroExpander
    public export
    data MacroExpand : Type -> Type where
      Refer : String -> MacroExpand (Maybe Value)
      Execute : VM a -> MacroExpand a

      Pure : a -> MacroExpand a
      Bind : MacroExpand a -> (a -> MacroExpand b) -> MacroExpand b
      Fail : String -> MacroExpand a

  namespace Compiler
    public export
    data Compile : Type -> Type where
      Do : Inst Value -> Compile ()
      Refer : String -> Compile (Maybe Value)
      Block : Compile () -> Compile (Code Value)

      Pure : a -> Compile a
      Bind : Compile a -> (a -> Compile b) -> Compile b
      Fail : String -> Compile a

  public export
  record Context where
    constructor MkContext
    topLevel : Env Value
    builtins : SortedMap String Builtin

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

namespace VM
  export
  Functor VM where
    map f a = Bind a (\a' => Pure (f a'))

  export
  Applicative VM where
    pure = Pure
    (<*>) f a = Bind f (\f' => Bind a (\a' => Pure (f' a')))

  export
  Monad VM where
    (>>=) = Bind

  export
  pop : VM Value
  pop =
    case !Pop of
      Just value => Pure value
      Nothing => Fail "Internal error: inconsistent stack"

  export
  define : String -> Value -> VM ()
  define name value =
    case !(Define name value) of
      Left err => Fail $ "Evaluation error: cannot define variable: " ++ err
      Right () => pure ()

  export
  set : String -> Value -> VM ()
  set name value =
    case !(Set name value) of
      Left err => Fail $ "Evaluation error: cannot set variable: " ++ err
      Right () => pure ()

  export
  applyClosure : Closure -> List Value -> VM ()
  applyClosure closure args = do
    Enter (env closure) (body closure)
    case Pattern.bind (pattern closure) args of
      Left err => Fail $ "Evaluation error: cannot call closure: " ++ err
      Right mapping => traverse_ (uncurry define) mapping

  export
  apply : Value -> List Value -> VM ()
  apply (Pure (NBuiltin builtin)) args = run builtin args
  apply (Pure (NFun closure)) args = applyClosure closure args
  apply f args = Fail $ "Evaluation error: cannot call " ++ show f

  export
  inst : Inst Value -> VM ()
  inst (Ldc constant) = Push constant
  inst (Ldv variable) =
    case !(Get variable) of
      Right x => Push x
      Left err => Fail $ "Evaluation error: undefined variable: " ++ err
  inst (Ldf pat code) = Push $ Pure $ NFun $ MkClosure !CaptureEnv pat code
  inst (Ldm pat code) = Push $ Pure $ NMacro $ MkClosure !CaptureEnv pat code
  inst (Ldb name) =
    case !(LoadBuiltin name) of
      Just builtin => Push $ Pure $ NBuiltin builtin
      Nothing => Fail $ "Evaluation error: unsupported builtin: " ++ name
  inst (Sel thenc elsec) =
    case !pop of
      Bool False => Enter !CaptureEnv elsec
      _ => Enter !CaptureEnv thenc
  inst (App argc) = do
    revArgs <- sequence $ replicate argc pop
    f <- pop
    apply f (reverse revArgs)
  inst Leave =
    case !Leave of
      Right _ => pure ()
      Left () => Fail "Internal error: inconsistent dump"
  inst Pop = ignore pop
  inst (Def name) = define name !pop
  inst (Set name) = set name !pop

  export
  instCycle : VM Value
  instCycle =
    case !Next of
      Just i => inst i *> instCycle
      Nothing => pop

namespace MacroExpander
  export
  Functor MacroExpand where
    map f a = Bind a (\a' => Pure (f a'))

  export
  Applicative MacroExpand where
    pure = Pure
    (<*>) f a = Bind f (\f' => Bind a (\a' => Pure (f' a')))

  export
  Monad MacroExpand where
    (>>=) = Bind

  export
  refer : Value -> MacroExpand (Maybe Value)
  refer (Sym sym) = Refer sym
  refer _ = pure Nothing

  mutual
    export
    expand : Bool -> Value -> MacroExpand Value
    expand recurse expr with (cast {to = ValueList} expr)
      | Proper (m :: args) =
          case !(refer m) of
            Just (Pure (NSyntax syn)) =>
              if recurse
                then pure $ m :: foldr (::) Nil !(macroExpand syn args)
                else ifThenElse recurse expandChildren pure expr
            Just (Pure (NMacro closure)) => do
              expr <- Execute (applyClosure closure args *> instCycle)
              ifThenElse recurse (expand True) pure expr
            _ => ifThenElse recurse expandChildren pure expr
      | _ = ifThenElse recurse expandChildren pure expr

    export
    expandChildren : Value -> MacroExpand Value
    expandChildren (car :: cdr) = [| expand True car :: expandChildren cdr |]
    expandChildren expr = pure expr

namespace Compiler
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
    load : Value -> Compile ()
    load (Sym sym) = Do $ Ldv sym
    load (f :: xs) with (cast {to = ValueList} xs)
      | Proper args =
          case !(refer f) of
            Just (Pure (NSyntax syn)) => compile syn args
            _ => do
              load f
              traverse_ load args
              Do $ App (length args)
      | Improper _ = Fail $ "Compile error: improper list: " ++ show (f :: xs)
    load x = Do $ Ldc x

