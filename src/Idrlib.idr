module Idrlib

import Data.IORef
import System
import Idrlisp

%default covering

mod' : Double -> Double -> Double
mod' x y =
  case cast {to = Integer} y of
    0 => 0.0 / 0.0
    y' => cast (assert_total (cast x `mod` y'))

dropPure : Value -> VM (Sexp ())
dropPure = traverse $ const $ Fail "comparing impure values are unsupported"

infixl 0 ->>

data BuiltinClause : Type where
  (->>) : (sig : ValueArgsSignature) -> (handle : SignatureType sig -> VM ()) -> BuiltinClause

signature : String -> BuiltinClause -> String
signature name (sig ->> _) = show (Sym name :: sig)

mkBuiltin : String -> List BuiltinClause -> (String, Builtin)
mkBuiltin name clauses = (name, MkBuiltin (run clauses))
  where
    run : List BuiltinClause -> List Value -> VM ()
    run ((sig ->> handle) :: ps) args =
      case match sig args of
        Just x => handle x
        Nothing => run ps args
    run [] _ =
      Fail $ "Evaluation error: expected " ++ concat (intersperse " or " (map (signature name) clauses))

mkBuiltinGensym : String -> IO (String, Builtin)
mkBuiltinGensym name = do
  idRef <- newIORef 0
  pure $ mkBuiltin name
    [ [] ->> \() => do
        id <- Action $ readIORef idRef
        let id' = succ id
        Action $ writeIORef idRef id'
        Push $ Sym $ "#sym." ++ show id'
    ]

mkBuiltinTest : String -> ValueSignature -> (String, Builtin)
mkBuiltinTest name sig = mkBuiltin name
  [ [Any "expr"] ->> \expr => Push $ Bool $ isJust $ match sig expr
  ]

mkBuiltinCompare : String -> (Double -> Double -> Bool) -> (String -> String -> Bool) -> (String, Builtin)
mkBuiltinCompare name fcmp scmp = mkBuiltin name
  [ [] ->> \() => Push $ Bool True
  , (Num "num" :: Rest (Num "nums")) ->> handle fcmp
  , (Str "str" :: Rest (Str "strs")) ->> handle scmp
  ]
  where
    handle : (b -> b -> Bool) -> (b, List b) -> VM ()
    handle cmp (x, xs) = Push $ Bool $ all id $ zipWith cmp (x :: xs) xs

mkBuiltinCont : String -> Cont -> (String, Builtin)
mkBuiltinCont name cont = mkBuiltin name
  [ [] ->> \() => RestoreCont cont *> Push Nil
  , [Any "x"] ->> \x => RestoreCont cont *> Push x
  , (Any "x" :: Rest (Any "xs")) ->> \xs => Fail "Evaluation error: multiple values are not implemented"
  ]

export
initIdrlib : List String -> IO (List (String, Builtin))
initIdrlib args = pure
  [ mkBuiltin "cons"
      [ [Any "car", Any "cdr"] ->> \(car, cdr) => Push (car :: cdr)
      ]

  , mkBuiltin "exit"
      [ [] ->> \() => Action $ exit 0
      , [Num "exitcode"] ->> \c => Action $ exit (cast c)
      ]
  , mkBuiltin "error"
      [ [] ->> \() => Fail "Evaluation error: error called"
      , [Str "msg"] ->> \msg => Fail $ "Evaluation error: " ++ msg
      ]

  , !(mkBuiltinGensym "gensym")

  , mkBuiltin "car"
      [ [(Any "car" :: Any "cdr")] ->> \(car, _) => Push car
      ]
  , mkBuiltin "cdr"
      [ [(Any "cdr" :: Any "cdr")] ->> \(_, cdr) => Push cdr
      ]

  , mkBuiltin "apply"
      [ [Any "f", List (Any "arguments")] ->> \(f, args) => apply f args
      ]

  , mkBuiltinTest "num?" (Num "")
  , mkBuiltinTest "sym?" (Sym "")
  , mkBuiltinTest "str?" (Str "")
  , mkBuiltinTest "cons?" (Any "" :: Any "")
  , mkBuiltinTest "nil?" Nil
  , mkBuiltinTest "bool?" (Bool "")
  , mkBuiltinTest "proc?" (Builtin "" `Or` Fun "")
  , mkBuiltinTest "meta?" (Syntax "" `Or` Macro "")
  -- , mkBuiltinTest "port?" []
  -- , mkBuiltinTest "vec?" []

  , mkBuiltin "+"
      [ Rest (Num "nums") ->> \nums => Push $ Num $ sum nums
      ]
  , mkBuiltin "-"
      [ [Num "num"] ->> \num => Push $ Num $ negate num
      , (Num "num" :: Rest (Num "nums")) ->> \(num, nums) => Push $ Num $ foldl (-) num nums
      ]
  , mkBuiltin "*"
      [ Rest (Num "nums") ->> \nums => Push $ Num $ product nums
      ]
  , mkBuiltin "/"
      [ [Num "num"] ->> \num => Push $ Num $ recip num
      , (Num "num" :: Rest (Num "nums")) ->> \(num, nums) => Push $ Num $ foldl (/) num nums
      ]
  , mkBuiltin "%"
      [ (Num "num" :: Rest (Num "nums")) ->> \(num, nums) => Push $ Num $ foldl mod' num nums
      ]

  , mkBuiltin "="
      [ [] ->> \() => Push $ Bool True
      , (Any "x" :: Rest (Any "xs")) ->> \(x, xs) => do
          x <- dropPure x
          xs <- traverse dropPure xs
          Push $ Bool $ all (== x) xs
      ]

  , mkBuiltinCompare "<" (<) (<)
  , mkBuiltinCompare ">" (>) (>)
  , mkBuiltinCompare "<=" (<=) (<=)
  , mkBuiltinCompare ">=" (>=) (>=)

  , mkBuiltin "call/cc"
      [ [Any "f"] ->> \f => do
          cont <- CaptureCont
          apply f [Pure (NBuiltin (snd (mkBuiltinCont "continuation" cont)))]
      ]

  , mkBuiltin "never"
      [ (Any "f" :: Rest (Any "arguments")) ->> \(f, args) => DropCont *> apply f args
      ]
  ]