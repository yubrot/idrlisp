module Test.Pattern

import Data.SortedMap
import Test.Assertions
import Idrlisp.Pattern as Pattern
import Idrlisp.Sexp

build' : Sexp () -> Either (Sexp ()) Pattern
build' = Pattern.build

bind' : List String -> Maybe String -> List (Sexp ()) -> Either String (List (String, Sexp ()))
bind' f r args = toList <$> Pattern.bind (MkPattern f r) args

export
test : IO ()
test = describe "Test.Pattern" $ do
  describe "Showing pattern" $ do
    MkPattern [] Nothing
      `shouldShow` "()"
    MkPattern ["x"] Nothing
      `shouldShow` "(x)"
    MkPattern ["x", "y"] Nothing
      `shouldShow` "(x y)"
    MkPattern [] (Just "a")
      `shouldShow` "a"
    MkPattern ["x"] (Just "a")
      `shouldShow` "(x . a)"
    MkPattern ["x", "y"] (Just "a")
      `shouldShow` "(x y . a)"
  describe "Building pattern" $ do
    build' []
      `shouldBe` Right (MkPattern [] Nothing)
    build' [Sym "x"]
      `shouldBe` Right (MkPattern ["x"] Nothing)
    build' [Sym "x", Sym "y"]
      `shouldBe` Right (MkPattern ["x", "y"] Nothing)
    build' (Sym "a")
      `shouldBe` Right (MkPattern [] (Just "a"))
    build' (Sym "x" :: Sym "a")
      `shouldBe` Right (MkPattern ["x"] (Just "a"))
    build' (Sym "x" :: Sym "y" :: Sym "a")
      `shouldBe` Right (MkPattern ["x", "y"] (Just "a"))
    build' (Num 123)
      `shouldBe` Left (Num 123)
    build' [Sym "x", Str "f", Sym "y"]
      `shouldBe` Left (Str "f")
  describe "Binding pattern" $ do
    bind' [] Nothing []
      `shouldBe` Right []
    bind' ["x", "y"] Nothing [Num 1, Num 2]
      `shouldBe` Right [("x", Num 1), ("y", Num 2)]
    bind' ["x", "y"] (Just "z") [Num 1, Num 2]
      `shouldBe` Right [("x", Num 1), ("y", Num 2), ("z", [])]
    bind' ["x", "y"] (Just "z") [Num 1, Num 2, Num 3, Num 4]
      `shouldBe` Right [("x", Num 1), ("y", Num 2), ("z", [Num 3, Num 4])]
    bind' ["x", "y"] Nothing [Num 1]
      `shouldSatisfy` isLeft
    bind' ["x", "y"] (Just "z") [Num 1]
      `shouldSatisfy` isLeft
    bind' ["x", "y"] Nothing [Num 1, Num 2, Num 3]
      `shouldSatisfy` isLeft

