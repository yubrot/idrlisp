module Idrlib

import Data.IORef
import System
import Idrlisp
import Idrlisp.Vec

%default covering

mod' : Double -> Double -> Double
mod' x y =
  case cast {to = Integer} y of
    0 => 0.0 / 0.0
    y' => cast (assert_total (cast x `mod` y'))

dropPure : Value -> Maybe (Sexp ())
dropPure = traverse $ const Nothing

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

mkBuiltinArgs : String -> List String -> (String, Builtin)
mkBuiltinArgs name args = mkBuiltin name
  [ [] ->> \() => Push $ foldr (::) Sexp.Nil (map Sexp.Str args)
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
  , mkBuiltinTest "proc?" (Pure "" NBuiltin `Or` Pure "" NFun)
  , mkBuiltinTest "meta?" (Pure "" NSyntax `Or` Pure "" NMacro)
  , mkBuiltinTest "port?" (Pure "" NPort)
  , mkBuiltinTest "vec?" (Pure "" NVec)

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
          let x' = dropPure x
          let xs' = map dropPure xs
          Push $ Bool $ all isJust xs' && all (== x') xs'
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

  -- String support has limitations. please see README.md
  , mkBuiltin "str"
      [ Rest (Num "bytes") ->> \chars =>
          Push $ Str $ pack [chr $ cast c | c <- the (List Double) chars]
      ]
  , mkBuiltin "str-ref"
      [ [Str "string", Num "index"] ->> \(str, index) =>
          let i = the Int $ cast index in
          case i < 0 || i >= cast (length str) of
            True => Push $ Nil
            False => Push $ Num $ cast $ ord $ assert_total (strIndex str i)
      ]
  , mkBuiltin "str-bytesize"
      [ [Str "string"] ->> \str => Push $ Num $ cast $ length str
      ]
  , mkBuiltin "str-concat"
      [ Rest (Str "strings") ->> \strs => Push $ Str $ concat strs
      ]
  , mkBuiltin "substr"
      [ [Str "string", Num "index", Num "bytesize"] ->> \(str, index, bytesize) =>
          let index'    = the Nat $ cast $ the Integer $ cast index
              bytesize' = the Nat $ cast $ the Integer $ cast bytesize
          in
          if index' < 0 || length str < index' + bytesize'
            then Fail "Evaluation error: substr: index out of range"
            else Push $ Str $ substr index' bytesize' str
      ]
  , mkBuiltin "sym->str"
      [ [Sym "symbol"] ->> \sym => Push $ Str sym
      ]
  , mkBuiltin "num->str"
      [ [Num "number"] ->> \num => Push $ Str $ show num
      ]
  , mkBuiltin "str->num"
      [ [Str "string"] ->> \str =>
          let n = the Double $ cast str in
          if n /= 0.0 || str == "0" || str == "0.0"
            then Push $ Num n
            else Push Nil
      ]

  , mkBuiltin "vec"
      [ Rest (Any "items") ->> \items => do
          vec <- Action $ Vec.fromList items
          Push $ Pure $ NVec vec
      ]
  , mkBuiltin "vec-make"
      [ [Num "length", Any "init"] ->> \(length, init) => do
          vec <- Action $ Vec.new (cast $ the Integer $ cast length) init
          Push $ Pure $ NVec vec
      ]
  , mkBuiltin "vec-ref"
      [ [Pure "vec" NVec, Num "index"] ->> \(vec, index) =>
          case !(Action $ Vec.read (cast $ the Integer $ cast index) vec) of
            Nothing => Push Nil
            Just x => Push x
      ]
  , mkBuiltin "vec-length"
      [ [Pure "vec" NVec] ->> \vec => Push $ Num $ cast $ Vec.length vec
      ]
  , mkBuiltin "vec-set!"
      [ [Pure "vec" NVec, Num "index", Any "item"] ->> \(vec, index, item) =>
          case !(Action $ Vec.write (cast $ the Integer $ cast index) item vec) of
            True => Push Nil
            False => Fail "Evaluation error: vec-set!: index out of range"
      ]
  , mkBuiltin "vec-copy!"
      [ [Pure "dest" NVec, Num "dest-start", Pure "src" NVec, Num "src-start", Num "length"] ->>
          \(dest, destStart, src, srcStart, len) =>
            let destStart' = cast $ the Integer $ cast destStart
                srcStart' = cast $ the Integer $ cast srcStart
                len' = cast $ the Integer $ cast len
            in
            case !(Action $ Vec.copy src srcStart' dest destStart' len') of
              True => Push Nil
              False => Fail "Evaluation error: vec-copy!: index out of range"
      ]

  , mkBuiltin "open"
      [ [Str "filepath", Str "mode"] ->> \(filepath, mode) => do
          mode <- case mode of
            "w" => pure WriteTruncate
            "r" => pure Read
            m => Fail $ "Evaluation error: open: unsupported mode: " ++ m
          result <- Action $ openFile filepath mode
          case result of
            Left err => Push $ Bool False :: Str (show err)
            Right file => Push $ Bool True :: Pure (NPort file)
      ]
  , mkBuiltin "close"
      [ [Pure "port" NPort] ->> \file => do
          Action $ closeFile file
          Push $ Bool True :: Nil
      ]

  , mkBuiltin "stdin" [[] ->> \() => Push $ Pure $ NPort stdin]
  , mkBuiltin "stdout" [[] ->> \() => Push $ Pure $ NPort stdout]
  , mkBuiltin "stderr" [[] ->> \() => Push $ Pure $ NPort stderr]

  , mkBuiltin "read-byte"
      [ [Pure "port" NPort] ->> \file => do
          result <- Action $ fgetc file
          case result of
            Left err => Push $ Bool False :: Str (show err)
            Right ch =>
              if the Int (cast ch) == 0
                then Push $ Bool True :: Sym "eof"
                else Push $ Bool True :: Num (cast $ the Int $ cast ch)
      ]
  , mkBuiltin "read-str"
      [ [Num "bytesize", Pure "port" NPort] ->> \(bytesize, file) => do
          result <- Action $ fGetChars file (cast bytesize)
          case result of
            Left err => Push $ Bool False :: Str (show err)
            Right s =>
              case length s of
                Z => Push $ Bool True :: Sym "eof"
                _ => Push $ Bool True :: Str s
      ]
  , mkBuiltin "read-line"
      [ [Pure "port" NPort] ->> \file => do
          result <- Action $ fGetLine file
          case result of
            Left err => Push $ Bool False :: Str (show err)
            Right s =>
              case length s of
                Z => Push $ Bool True :: Sym "eof"
                S k => Push $ Bool True :: Str (substr 0 k s)
      ]

  , mkBuiltin "write-byte"
      [ [Num "byte", Pure "port" NPort] ->> \(ch, file) => do
          result <- Action $ fPutStr file (cast ch)
          case result of
            Left err => Push $ Bool False :: Str (show err)
            Right () => Push $ Bool True :: Num 1.0
      ]
  , mkBuiltin "write-str"
      [ [Str "str", Pure "port" NPort] ->> \(str, file) => do
          result <- Action $ fPutStr file str
          case result of
            Left err => Push $ Bool False :: Str (show err)
            Right () => Push $ Bool True :: Num (cast $ the Int $ cast $ length str)
      ]
  , mkBuiltin "write-line"
      [ [Str "str", Pure "port" NPort] ->> \(str, file) => do
          result <- Action $ fPutStrLn file str
          case result of
            Left err => Push $ Bool False :: Str (show err)
            Right () => Push $ Bool True :: Num (cast $ the Int $ cast $ length str + 1)
      ]

  , mkBuiltin "flush"
      [ [Pure "port" NPort] ->> \file => do
          Action $ fflush file
          Push $ Bool True :: Nil
      ]

  , mkBuiltinArgs "args" args

  , mkBuiltin "eval"
      [ [Any "expr"] ->> \expr => do
          ctx <- CaptureContext
          result <- Action $ evalExpr ctx expr
          case result of
            Left err => Push $ Bool False :: Str err
            Right v => Push $ Bool True :: v
      ]
  , mkBuiltin "macroexpand"
      [ [Any "expr"] ->> \expr => do
          ctx <- CaptureContext
          result <- Action $ macroExpandExpr ctx True expr
          case result of
            Left err => Push $ Bool False :: Str err
            Right v => Push $ Bool True :: v
      ]
  , mkBuiltin "macroexpand-1"
      [ [Any "expr"] ->> \expr => do
          ctx <- CaptureContext
          result <- Action $ macroExpandExpr ctx False expr
          case result of
            Left err => Push $ Bool False :: Str err
            Right v => Push $ Bool True :: v
      ]
  ]

