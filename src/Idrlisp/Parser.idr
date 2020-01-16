module Idrlisp.Parser

import Text.Parser
import Idrlisp.Sexp
import Idrlisp.Lexer

%default total

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
    [ list LPAREN RPAREN
    , list LBRACK RBRACK
    , quotes
    , Num <$> num
    , Sym <$> sym
    , Str <$> str
    , do term TRUE; pure $ Bool True
    , do term FALSE; pure $ Bool False
    ]

  quotes : Grammar Token True (SSyn a)
  quotes =
    do
      f <- quote
      commit
      x <- s
      pure $ f x
    where
      -- TODO: Why these function liftings are required for the totality checker?
      quote : Grammar Token True (SSyn a -> SSyn a)
      quote = choice
        [ do term QUOTE; pure Quote
        , do term QUASIQUOTE; pure Quasiquote
        , do term UNQUOTE; pure Unquote
        , do term UNQUOTE_SPLICING; pure UnquoteSplicing
        ]

  list : Token -> Token -> Grammar Token True (SSyn a)
  list open close =
    do
      term open
      commit
      xs <- init
      y <- rest xs
      term close
      pure $ build xs y
    where
      -- TODO: ditto
      init : Grammar Token False (List (SSyn a))
      init = many s

      rest : List (SSyn a) -> Grammar Token False (Maybe (SSyn a))
      rest [] = pure Nothing
      rest _ = optional $ do term DOT; commit; s

      build : List (SSyn a) -> Maybe (SSyn a) -> SSyn a
      build [] _ = Nil
      -- TODO: Why these aren't total?
      build (x :: xs) Nothing = assert_total $ App (x :: xs)
      build (x :: xs) (Just y) = assert_total $ x :: xs :.: y

export
Parse True ty => Parse False (List ty) where
  grammar {ty} = many $ grammar {ty}

export
Parse True (SSyn a) where
  grammar = s

export
Parse True (Sexp a) where
  grammar = map cast $ grammar {ty = SSyn a}

