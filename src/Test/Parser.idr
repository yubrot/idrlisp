module Test.Parser

import Test.Assertions
import Idrlisp.Sexp
import Idrlisp.Parser

export
test : IO ()
test = describe "Test.Parser" $ do
  describe "parseToEnd" $ do
    let parseSyn = parseToEnd {ty = SSyn ()}
    parseSyn "123"
      `shouldBe` Right (Num 123)
    parseSyn " \n\t ; comment \n 3.14  "
      `shouldBe` Right (Num 3.14)
    parseSyn "*foo-bar+baz"
      `shouldBe` Right (Sym "*foo-bar+baz")
    parseSyn (show "Hello, World!\n")
      `shouldBe` Right (Str "Hello, World!\n")
    parseSyn "()"
      `shouldBe` Right Nil
    parseSyn "(#t)"
      `shouldBe` Right (App [Bool True])
    parseSyn "(#t #f)"
      `shouldBe` Right (App [Bool True, Bool False])
    parseSyn "(#t #f . 0)"
      `shouldBe` Right ([Bool True, Bool False] :.: Num 0)
    parseSyn "(#t #f . 0 1)"
      `shouldSatisfy` isLeft
    parseSyn "(foo [bar baz])"
      `shouldBe` Right (App [Sym "foo", App [Sym "bar", Sym "baz"]])
    parseSyn "`(foo ,bar ,@(hoge 'fuga))"
      `shouldBe` Right (Quasiquote (App [Sym "foo", Unquote (Sym "bar"), UnquoteSplicing (App [Sym "hoge", Quote (Sym "fuga")])]))
    parseToEnd {ty = List (SSyn ())} "123 () foo ; test \n \"bar\""
      `shouldBe` Right [Num 123.0, Nil, Sym "foo", Str "bar"]

