module Idrlisp.Parser

import Text.Parser
import Idrlisp.Sexp
import Idrlisp.Lexer

%default covering

public export
interface Parse (c : Bool) (ty : Type) | ty where
  grammar : Grammar Token c ty

export
parseToEnd : Parse c ty => String -> Either String ty
parseToEnd {c} {ty} input =
  case lex input of
    Left (line, col, _) =>
      Left $ "Parse error at " ++ show line ++ ":" ++ show col ++ ": lexical error"
    Right tokens =>
      -- If we place parseToEnd in interface, grammar reports that the implementation is missing
      case parse (grammar {c} {ty} <* eof) tokens of
        Left (Error msg _) => Left $ "Parse error: " ++ msg
        Right (x, _) => Right x

namespace Terminal
  term : Token -> Grammar Token True ()
  term t = terminal $ \t' => toMaybe (t == t') ()

  num : Grammar Token True Double
  num = terminal $ \t =>
    case t of
      NUM x => Just x
      _ => Nothing

  sym : Grammar Token True String
  sym = terminal $ \t =>
    case t of
      SYM x => Just x
      _ => Nothing

  str : Grammar Token True String
  str = terminal $ \t =>
    case t of
      STR x => Just x
      _ => Nothing

mutual
  s : Grammar Token True (SSyn a)
  s = choice
    [ do term LPAREN; xs <- ss; term RPAREN; pure xs
    , do term LBRACK; xs <- ss; term RBRACK; pure xs
    , sQuoted
    , Num <$> num
    , Sym <$> sym
    , Str <$> str
    , do term TRUE; pure $ Bool True
    , do term FALSE; pure $ Bool False
    ]

  sQuoted : Grammar Token True (SSyn a)
  sQuoted = choice
    [ do term QUOTE; Quote <$> s
    , do term QUASIQUOTE; Quasiquote <$> s
    , do term UNQUOTE; Unquote <$> s
    , do term UNQUOTE_SPLICING; UnquoteSplicing <$> s
    ]

  ss : Grammar Token False (SSyn a)
  ss {a} = do
    xs <- many $ s {a}
    case xs of
      [] => pure Nil
      x :: xs => do
        y <- optional $ term DOT *> s {a}
        case y of
          Nothing => pure $ App (x :: xs)
          Just y => pure $ x :: xs :.: y

export
Parse True ty => Parse False (List ty) where
  grammar {ty} = many $ grammar {ty}

export
Parse True (SSyn a) where
  grammar = s

export
Parse True (Sexp a) where
  grammar = map cast $ grammar {ty = SSyn a}

