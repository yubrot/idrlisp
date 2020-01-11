module Idrlisp.Error

%default total

public export
data Error
  = UndefinedVariable String

export
Show Error where
  show (UndefinedVariable x) = "Undefined variable: " ++ x

export
Eq Error where
  (==) (UndefinedVariable x) (UndefinedVariable y) = x == y
  (==) _ _ = False

