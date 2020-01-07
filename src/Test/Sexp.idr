module Test.Sexp

import Test.Assertions
import Idrlisp.Sexp

castTestCases : List (Sexp (), SSyn ())
castTestCases =
  [ (Num 123, Num 123)
  , (Sym "foo", Sym "foo")
  , (Str "hello", Str "hello")
  , (Bool True, Bool True)
  , (Bool True :: Bool False, [Bool True] :.: Bool False)
  , (Bool True :: Bool False :: Num 0 :: Num 1, [Bool True, Bool False, Num 0] :.: Num 1)
  , ([Sym "id"], App [Sym "id"])
  , ([Sym "f", Sym "a", Sym "b"], App [Sym "f", Sym "a", Sym "b"])
  , ([Sym "quote", Sym "foo"], Quote (Sym "foo"))
  , (
      [Sym "quasiquote", [
        Sym "bar",
        [Sym "unquote", Sym "baz"],
        [Sym "unquote-splicing", [Sym "hoge", Sym "fuga"]]
      ]],
      Quasiquote (App [
        Sym "bar",
        Unquote (Sym "baz"),
        UnquoteSplicing (App [Sym "hoge", Sym "fuga"])
      ])
    )
  , (Nil, Nil)
  , (Pure (), Pure ())
  ]

showTestCases : List (String, SSyn ())
showTestCases =
  [ ("123", Num 123)
  , ("3.14", Num 3.14)
  , ("foo", Sym "foo")
  , ("unquote-splicing", Sym "unquote-splicing")
  , ("\"Text\"", Str "Text")
  , ("\"Hello, World!\\n\"", Str "Hello, World!\n")
  , ("#t", Bool True)
  , ("()", Nil)
  , ("'foo", Quote (Sym "foo"))
  , ("`(bar ,baz ,@(hoge fuga))", Quasiquote (App [Sym "bar", Unquote (Sym "baz"), UnquoteSplicing (App [Sym "hoge", Sym "fuga"])]))
  , ("(f)", App [Sym "f"])
  , ("(f a b)", App [Sym "f", Sym "a", Sym "b"])
  , ("(#t . #f)", [Bool True] :.: Bool False)
  , ("(#t #f 0 . 1)", [Bool True, Bool False, Num 0] :.: Num 1)
  , ("()", Pure ())
  ]

export
test : IO ()
test = describe "Test.Sexp" $ do
  describe "Casting from Sexp to SSyn" $
    for_ castTestCases (\t => cast (fst t) `shouldBe` snd t)
  describe "Casting from SSyn to Sexp" $
    for_ castTestCases (\t => cast (snd t) `shouldBe` fst t)
  describe "Showing SSyn" $
    for_ showTestCases (\t => snd t `shouldShow` fst t)

