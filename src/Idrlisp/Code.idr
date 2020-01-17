module Idrlisp.Code

import Control.Monad.State
import Idrlisp.Pattern

%default total

mutual
  public export
  data Inst a
    = Ldc a
    | Ldv String
    | Ldf Pattern (Code a)
    | Ldm Pattern (Code a)
    | Ldb String
    | Sel (Code a) (Code a)
    | App Nat
    | Leave
    | Pop
    | Def String
    | Set String

  export
  data Code a = MkCode (List (Inst a))

export
empty : Code a
empty = MkCode []

export
tailPosition : Code a -> Bool
tailPosition (MkCode [Leave]) = True
tailPosition (MkCode _) = False

export
singleton : Inst a -> Code a
singleton = MkCode . pure

export
next : Code a -> Maybe (Inst a, Code a)
next (MkCode []) = Nothing
next (MkCode (x :: xs)) = Just (x, MkCode xs)

export
(++) : Code a -> Code a -> Code a
(++) (MkCode x) (MkCode y) = MkCode (x ++ y)

mutual
  showCode : Show a => String -> Code a -> State Nat (String, String)
  showCode header (MkCode insts) = do
    id <- get
    modify succ
    let label = "[" ++ show id ++ " " ++ header ++ "]"
    (body, innerCodes) <- showInsts insts
    pure (label, label ++ "\n" ++ body ++ innerCodes)

  showInsts : Show a => List (Inst a) -> State Nat (String, String)
  showInsts [] = pure ("", "")
  showInsts (inst :: rest) = do
    (inst, innerCodes) <- showInst inst
    (body, innerCodes') <- showInsts rest
    pure ("  " ++ inst ++ "\n" ++ body, innerCodes ++ innerCodes')

  showInst : Show a => Inst a -> State Nat (String, String)
  showInst (Ldc value) = pure ("ldc " ++ show value, "")
  showInst (Ldv var) = pure ("ldv " ++ var, "")
  showInst (Ldf pat body) = do
    (label, innerCodes) <- showCode ("fun " ++ show pat) body
    pure ("ldf " ++ label, innerCodes)
  showInst (Ldm pat body) = do
    (label, innerCodes) <- showCode ("macro " ++ show pat) body
    pure ("ldm " ++ label, innerCodes)
  showInst (Ldb name) = pure ("ldb " ++ name, "")
  showInst (Sel thenc elsec) = do
    (thenl, innerCodes) <- showCode "then" thenc
    (elsel, innerCodes') <- showCode "else" elsec
    pure ("sel " ++ thenl ++ " " ++ elsel, innerCodes ++ innerCodes')
  showInst (App arity) = pure ("app " ++ show arity, "")
  showInst Leave = pure ("leave", "")
  showInst Pop = pure ("pop", "")
  showInst (Def name) = pure ("def " ++ name, "")
  showInst (Set name) = pure ("set " ++ name, "")

export
Show a => Show (Code a) where
  show code = evalState (snd <$> showCode "entry" code) 0

