module Idrlisp.Signature

import Idrlisp.Sexp
import Idrlisp.Pattern as Pattern

%default total

public export
data Signature
  = Any String
  | Num String
  | Sym String
  | Str String
  | Bool String
  | Nil
  | (::) Signature Signature
  | List Signature
  | Pat String

namespace Args
  public export
  data ArgsSignature
    = Nil
    | (::) Signature ArgsSignature
    | Rest Signature

export
Cast Signature (Sexp ()) where
  cast (Any x) = Sym x
  cast (Num x) = Sym x
  cast (Sym x) = Sym x
  cast (Str x) = Sym x
  cast (Bool x) = Sym x
  cast Nil = Nil
  cast (x :: y) = cast x :: cast y
  cast (List x) = [cast x, Sym "..."]
  cast (Pat x) = Sym x

export
Cast ArgsSignature (Sexp ()) where
  cast Nil = Nil
  cast (x :: y) = cast x :: cast y
  cast (Rest x) = [cast x, Sym "..."]

export
covering
Show Signature where
  show x = show (the (Sexp ()) (cast x))

export
covering
Show ArgsSignature where
  show x = show (the (Sexp ()) (cast x))

public export
interface Match a s | a where
  SignatureType : a -> Type
  match : (sig : a) -> s -> Maybe (SignatureType sig)

public export
Match Signature (Sexp ()) where
  SignatureType (Any _) = Sexp ()
  SignatureType (Num _) = Double
  SignatureType (Sym _) = String
  SignatureType (Str _) = String
  SignatureType (Bool _) = Bool
  SignatureType Nil = ()
  SignatureType (x :: y) =
    case y of
      Nil => SignatureType x
      y' => (SignatureType x, SignatureType y')
  SignatureType (List x) = List (SignatureType x)
  SignatureType (Pat _) = Pattern

  match (Any _) x = Just x
  match (Num _) (Num x) = Just x
  match (Num _) _ = Nothing
  match (Sym _) (Sym x) = Just x
  match (Sym _) _ = Nothing
  match (Str _) (Str x) = Just x
  match (Str _) _ = Nothing
  match (Bool _) (Bool x) = Just x
  match (Bool _) _ = Nothing
  match Nil [] = Just ()
  match Nil _ = Nothing
  match (car :: cdr) (x :: y) = do
    x' <- match car x
    y' <- match cdr y
    Just $ case cdr of
      Nil => x'
      -- It seems that Idris type checker cannot specialize `SignatureType (car :: cdr)`.
      _ => believe_me (x', y')
  match (car :: cdr) _ = Nothing
  match (List a) xs with (cast {to = SList ()} xs)
    | Proper xs' = traverse (match a) xs'
    | Improper x = Nothing
  match (Pat _) x =
    case Pattern.build x of
      Left _ => Nothing
      Right p => Just p

public export
Match ArgsSignature (List (Sexp ())) where
  SignatureType Nil = ()
  SignatureType (x :: y) =
    case y of
      Nil => SignatureType x
      y' => (SignatureType x, SignatureType y')
  SignatureType (Rest x) = List (SignatureType x)

  match Nil [] = Just ()
  match Nil _ = Nothing
  match (car :: cdr) (x :: y) = do
    x' <- match car x
    y' <- match cdr y
    Just $ case cdr of
      Nil => x'
      -- Same as Match Signature, Idris type checker cannot specialize `SignatureType (car :: cdr)`.
      _ => believe_me (x', y')
  match (car :: cdr) _ = Nothing
  match (Rest a) xs = traverse (match a) xs

