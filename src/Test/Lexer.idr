module Test.Lexer

import Test.Assertions
import Idrlisp.Lexer

%default covering

export
test : IO ()
test = describe "Idrlisp.Lexer" $ do
  describe "lex" $ do
    lex ""
      `shouldBe` Right []
    lex "    ; hello  \n\n  "
      `shouldBe` Right []
    lex "()"
      `shouldBe` Right [LPAREN, RPAREN]
    lex ")[(]"
      `shouldBe` Right [RPAREN, LBRACK, LPAREN, RBRACK]
    lex "#t.#f"
      `shouldBe` Right [TRUE, DOT, FALSE]
    lex "123"
      `shouldBe` Right [NUM 123]
    lex "3.14"
      `shouldBe` Right [NUM 3.14]
    lex "-16.5"
      `shouldBe` Right [NUM (-16.5)]
    lex "64e+3"
      `shouldBe` Right [NUM 64000]
    lex "foo"
      `shouldBe` Right [SYM "foo"]
    lex "hello-1world*"
      `shouldBe` Right [SYM "hello-1world*"]
    lex (show "Hello, World!")
      `shouldBe` Right [STR "Hello, World!"]
    lex (show "Escape\nsequence\tEscape\"sequence\\")
      `shouldBe` Right [STR "Escape\nsequence\tEscape\"sequence\\"]
    lex "`(foo ,bar ,@(hoge 'fuga))"
      `shouldBe` Right [QUASIQUOTE, LPAREN, SYM "foo", UNQUOTE, SYM "bar", UNQUOTE_SPLICING, LPAREN, SYM "hoge", QUOTE, SYM "fuga", RPAREN, RPAREN]

