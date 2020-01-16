module Idrlisp.Pattern

import Idrlisp.Sexp

%default total

public export
record Pattern where
  constructor MkPattern
  fixed : List String
  rest : Maybe String

export
Eq Pattern where
  (==) x y = fixed x == fixed y && rest x == rest y

export
Show Pattern where
  show x =
    let fixed = map Sym (fixed x) in
    let rest = map Sym (rest x) in
    assert_total $ show $ the (Sexp ()) $ foldr (::) (fromMaybe Nil rest) fixed

export
build : Sexp a -> Either (Sexp a) Pattern
build (Sym x) = Right $ MkPattern [] (Just x)
build Nil = Right $ MkPattern [] Nothing
build (Sym x :: s) = record { fixed $= (x::) } <$> build s
build (x :: s) = Left x
build s = Left s

export
bind : Pattern -> List (Sexp a) -> Either String (List (String, Sexp a))
bind pat xs = go (fixed pat) (rest pat) xs
  where
    go : List String -> Maybe String -> List (Sexp a) -> Either String (List (String, Sexp a))
    go (p :: ps) rest [] = Left "not enough arguments"
    go (p :: ps) rest (a :: args) = ((p, a) ::) <$> go ps rest args
    go [] Nothing [] = Right []
    go [] Nothing (a :: args) = Left "too much arguments"
    go [] (Just rest) args = Right [(rest, foldr (::) Nil args)]

