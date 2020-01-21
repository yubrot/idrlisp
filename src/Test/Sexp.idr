module Test.Sexp

import Test.Assertions
import Idrlisp.Sexp

%default covering

sexpAndSSynTestcases : List (Sexp (), SSyn ())
sexpAndSSynTestcases =
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

sexpAndSListTestcases : List (Sexp (), SList ())
sexpAndSListTestcases =
  [ (Num 123, Improper (Num 123))
  , ([], Proper [])
  , ([Num 1, Num 2], Proper [Num 1, Num 2])
  , (Num 1 :: Num 2 :: Num 3, Improper (Num 1 :: Num 2 :: Num 3))
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
test = describe "Idrlisp.Sexp" $ do
  describe "cast" $ do
    for_ sexpAndSSynTestcases (`shouldSatisfy` castIdentical)
    for_ sexpAndSListTestcases (`shouldSatisfy` castIdentical)
  describe "show" $ do
    for_ showTestCases (\t => snd t `shouldShow` fst t)

  let testSexp : Sexp Integer = [Pure 1, Pure 2 :: Pure 3]

  describe "map" $
    map succ testSexp
      `shouldBe` [Pure 2, Pure 3 :: Pure 4]
  describe "foldr" $
    foldr (::) Nil testSexp
      `shouldBe` [1, 2, 3]
  describe "traverse" $
    traverse (const (the (List Int) [0])) testSexp
      `shouldBe` [[Pure 0, Pure 0 :: Pure 0]]

