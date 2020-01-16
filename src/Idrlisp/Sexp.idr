module Idrlisp.Sexp

%default total

public export
data Sexp : Type -> Type where
  Num : Double -> Sexp a
  Sym : String -> Sexp a
  Str : String -> Sexp a -- For now we don't treat idrlisp strings as byte sequences.
  Bool : Bool -> Sexp a
  Nil : Sexp a
  (::) : (car : Sexp a) -> (cdr : Sexp a) -> Sexp a
  Pure : a -> Sexp a

public export
data SList a
  = Proper (List (Sexp a))
  | Improper (Sexp a)

namespace Syntax
  infixr 6 :.:

  public export
  data SSyn : Type -> Type where
    Num : Double -> SSyn a
    Sym : String -> SSyn a
    Str : String -> SSyn a
    Bool : Bool -> SSyn a
    Nil : SSyn a
    Quote : SSyn a -> SSyn a
    Quasiquote : SSyn a -> SSyn a
    Unquote : SSyn a -> SSyn a
    UnquoteSplicing : SSyn a -> SSyn a
    App : (xs : List (SSyn a)) -> {auto ok : NonEmpty xs} -> SSyn a
    (:.:) : (xs : List (SSyn a)) -> {auto ok : NonEmpty xs} -> SSyn a -> SSyn a
    Pure : a -> SSyn a

export
Eq a => Eq (Sexp a) where
  (==) (Num x) (Num y) = x == y
  (==) (Sym x) (Sym y) = x == y
  (==) (Str x) (Str y) = x == y
  (==) (Bool x) (Bool y) = x == y
  (==) Nil Nil = True
  (==) (x :: x') (y :: y') = x == y && x' == y'
  (==) (Pure x) (Pure y) = x == y
  (==) _ _ = False

export
Eq a => Eq (SList a) where
  (==) (Proper xs) (Proper ys) = xs == ys
  (==) (Improper x) (Improper y) = x == y
  (==) _ _ = False

export
Eq a => Eq (SSyn a) where
  x == y = assert_total (eq x y)
    where
      covering
      eq : SSyn a -> SSyn a -> Bool
      eq (Num x) (Num y) = x == y
      eq (Sym x) (Sym y) = x == y
      eq (Str x) (Str y) = x == y
      eq (Bool x) (Bool y) = x == y
      eq Nil Nil = True
      eq (Quote x) (Quote y) = x == y
      eq (Quasiquote x) (Quasiquote y) = x == y
      eq (Unquote x) (Unquote y) = x == y
      eq (UnquoteSplicing x) (UnquoteSplicing y) = x == y
      eq (App xs) (App ys) = xs == ys
      eq (xs :.: x) (ys :.: y) = xs == ys && x == y
      eq (Pure x) (Pure y) = x == y
      eq _ _ = False

export
Cast (Sexp a) (SList a) where
  cast (x :: xs) with (cast {to = SList a} xs)
    | Proper xs' = Proper (x :: xs')
    | Improper xs' = Improper (x :: xs')
  cast [] = Proper []
  cast x = Improper x

export
Cast (SList a) (Sexp a) where
  cast (Proper xs) = foldr (::) Nil xs
  cast (Improper x) = x

export
Cast (Sexp a) (SSyn a) where
  cast x = assert_total (cast x)
    where
      covering
      cast : Sexp a -> SSyn a
      cast (Num x) = Num x
      cast (Sym x) = Sym x
      cast (Str x) = Str x
      cast (Bool x) = Bool x
      cast Nil = Nil
      cast [Sym "quote", x] = Quote (cast x)
      cast [Sym "quasiquote", x] = Quasiquote (cast x)
      cast [Sym "unquote", x] = Unquote (cast x)
      cast [Sym "unquote-splicing", x] = UnquoteSplicing (cast x)
      cast (x :: y) with (cast y)
        | App ys = App (cast x :: ys)
        | Nil = App [cast x]
        | (ys :.: z) = cast x :: ys :.: z
        | y' = [cast x] :.: y'
      cast (Pure x) = Pure x

export
Cast (SSyn a) (Sexp a) where
  cast x = assert_total (cast x)
    where
      covering
      cast : SSyn a -> Sexp a
      cast (Num x) = Num x
      cast (Sym x) = Sym x
      cast (Str x) = Str x
      cast (Bool x) = Bool x
      cast Nil = Nil
      cast (Quote x) = [Sym "quote", cast x]
      cast (Quasiquote x) = [Sym "quasiquote", cast x]
      cast (Unquote x) = [Sym "unquote", cast x]
      cast (UnquoteSplicing x) = [Sym "unquote-splicing", cast x]
      cast (App xs) = foldr (::) Nil (map cast xs)
      cast (xs :.: x) = foldr (::) (cast x) (map cast xs)
      cast (Pure x) = Pure x

export
Show a => Show (SSyn a) where
  show x = assert_total (show' x)
    where
      covering
      show' : SSyn a -> String
      show' (Num x) = show x
      show' (Sym x) = x
      show' (Str x) = show x
      show' (Bool x) = if x then "#t" else "#f"
      show' Nil = "()"
      show' (Quote x) = "'" ++ show x
      show' (Quasiquote x) = "`" ++ show x
      show' (Unquote x) = "," ++ show x
      show' (UnquoteSplicing x) = ",@" ++ show x
      show' (App xs) = "(" ++ unwords (map show xs) ++ ")"
      show' (xs :.: x) = "(" ++ unwords (map show xs) ++ " . " ++ show x ++ ")"
      show' (Pure x) = show x

export
Show a => Show (Sexp a) where
  show x = show (the (SSyn a) (cast x))

export
Show a => Show (SList a) where
  show x = show (the (Sexp a) (cast x))

