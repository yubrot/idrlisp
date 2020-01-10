module Idrlisp.Lexer

import Text.Lexer

%default total

escapeChars : List (Char, Char)
escapeChars =
  [ ('\\', '\\')
  , ('t', '\t')
  , ('n', '\n')
  , ('"', '"')
  ]

public export
data Token
  = AMB
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | DOT
  | TRUE
  | FALSE
  | QUOTE
  | QUASIQUOTE
  | UNQUOTE
  | UNQUOTE_SPLICING
  | NUM Double
  | SYM String
  | STR String

export
Eq Token where
  (==) AMB AMB = True
  (==) LPAREN LPAREN = True
  (==) RPAREN RPAREN = True
  (==) LBRACK LBRACK = True
  (==) RBRACK RBRACK = True
  (==) DOT DOT = True
  (==) TRUE TRUE = True
  (==) FALSE FALSE = True
  (==) QUOTE QUOTE = True
  (==) QUASIQUOTE QUASIQUOTE = True
  (==) UNQUOTE UNQUOTE = True
  (==) UNQUOTE_SPLICING UNQUOTE_SPLICING = True
  (==) (NUM x) (NUM y) = x == y
  (==) (SYM x) (SYM y) = x == y
  (==) (STR x) (STR y) = x == y
  (==) _ _ = False

export
Show Token where
  show AMB = "AMB"
  show LPAREN = "LPAREN"
  show RPAREN = "RPAREN"
  show LBRACK = "LBRACK"
  show RBRACK = "RBRACK"
  show DOT = "DOT"
  show TRUE = "TRUE"
  show FALSE = "FALSE"
  show QUOTE = "QUOTE"
  show QUASIQUOTE = "QUASIQUOTE"
  show UNQUOTE = "UNQUOTE"
  show UNQUOTE_SPLICING = "UNQUOTE_SPLICING"
  show (NUM x) = "NUM " ++ show x
  show (SYM x) = "SYM " ++ x
  show (STR x) = "STR " ++ show x

tokenMap : TokenMap Token
tokenMap =
  [ (amb, const AMB)
  , (is '(', const LPAREN)
  , (is ')', const RPAREN)
  , (is '[', const LBRACK)
  , (is ']', const RBRACK)
  , (is '.', const DOT)
  , (exact "#t", const TRUE)
  , (exact "#f", const FALSE)
  , (is '\'', const QUOTE)
  , (is '`', const QUASIQUOTE)
  , (exact ",@", const UNQUOTE_SPLICING)
  , (is ',', const UNQUOTE)
  , (num, NUM . cast)
  , (sym, SYM)
  , (str, STR . unstr)
  ]
  where
    amb : Lexer
    amb = some (spaces <|> lineComment (is ';'))

    num : Lexer
    num = opt (oneOf "-+") <+> digits <+> opt frac <+> opt exp
      where
        frac = is '.' <+> opt digits
        exp = oneOf "eE" <+> opt (oneOf "-+") <+> digits

    sym : Lexer
    sym = (alpha <|> special) <+> many (alpha <|> digit <|> special)
      where
        special : Lexer
        special = some $ oneOf "!$%&*+-/:<=>?@^_~"

    str : Lexer
    str = is '"' <+> many (escapeSequence <|> isNot '"') <+> is '"'
      where
        escapeSequence : Lexer
        escapeSequence = escape '\\' $ oneOf $ pack $ map fst escapeChars

    unstr : String -> String
    unstr = pack . unescape . unquote . unpack
      where
        unquote : List Char -> List Char
        unquote ('"' :: s@(_ :: _)) = init s
        unquote s = s

        unescape : List Char -> List Char
        unescape ('\\' :: c :: s) = fromMaybe c (lookup c escapeChars) :: unescape s
        unescape (c :: s) = c :: unescape s
        unescape [] = []

export
lex : String -> Either (Int, Int, String) (List Token)
lex str =
  case lex tokenMap str of
    (tokens, (l, c, "")) => Right (mapMaybe dropAmb tokens)
    (_, fail) => Left fail
  where
    dropAmb : TokenData Token -> Maybe Token
    dropAmb t =
      case tok t of
        AMB => Nothing
        x => Just x

