module Test.Signature

import Test.Assertions
import Idrlisp.Sexp
import Idrlisp.Pattern
import Idrlisp.Signature

%default covering

matchSignature : (sig : Signature ()) -> Sexp () -> Maybe (SignatureType sig)
matchSignature = match

matchArgsSignature : (sig : ArgsSignature ()) -> List (Sexp ()) -> Maybe (SignatureType sig)
matchArgsSignature = match

signatureTest : IO ()
signatureTest = describe "Signature" $ do
  let any  : Signature () = Any "any"
  let num  : Signature () = Num "num"
  let sym  : Signature () = Sym "sym"
  let str  : Signature () = Str "str"
  let bool : Signature () = Bool "bool"
  let nil  : Signature () = Nil
  let vec2 : Signature () = [Num "x", Num "y"]
  let strs : Signature () = List (Str "strs")
  let pat  : Signature () = Pat "args"
  let numOrStr : Signature () = Or num str

  describe "show" $ do
    any `shouldShow` "any"
    num `shouldShow` "num"
    sym `shouldShow` "sym"
    str `shouldShow` "str"
    bool `shouldShow` "bool"
    nil `shouldShow` "()"
    vec2 `shouldShow` "(x y)"
    strs `shouldShow` "(strs ...)"
    pat `shouldShow` "args"
    numOrStr `shouldShow` "(or num str)"
  describe "match" $ do
    matchSignature any (Num 123)
      `shouldBe` Just (Sexp.Num 123)
    matchSignature any (Sym "foo")
      `shouldBe` Just (Sexp.Sym "foo")
    matchSignature num (Num 123)
      `shouldBe` Just 123
    matchSignature num (Str "foo")
      `shouldBe` Nothing
    matchSignature sym (Sym "foo")
      `shouldBe` Just "foo"
    matchSignature sym (Str "foo")
      `shouldBe` Nothing
    matchSignature str (Str "bar")
      `shouldBe` Just "bar"
    matchSignature str (Sym "bar")
      `shouldBe` Nothing
    matchSignature bool (Bool True)
      `shouldBe` Just True
    matchSignature bool (Sym "bar")
      `shouldBe` Nothing
    matchSignature nil []
      `shouldBe` Just ()
    matchSignature nil (Sym "bar")
      `shouldBe` Nothing
    matchSignature vec2 []
      `shouldBe` Nothing
    matchSignature vec2 [Num 123, Num 456]
      `shouldBe` Just (123, 456)
    matchSignature vec2 [Num 123]
      `shouldBe` Nothing
    matchSignature vec2 [Num 123, Num 456, Num 789]
      `shouldBe` Nothing
    matchSignature strs [Str "foo", Str "bar", Str "baz"]
      `shouldBe` Just ["foo", "bar", "baz"]
    matchSignature strs [Str "foo", Str "bar", Num 0]
      `shouldBe` Nothing
    matchSignature strs (Str "foo" :: Str "bar" :: Str "baz")
      `shouldBe` Nothing
    matchSignature pat (Sym "foo" :: Sym "bar" :: Sym "baz")
      `shouldBe` Just (MkPattern ["foo", "bar"] (Just "baz"))
    matchSignature pat [Sym "foo", Sym "bar", Sym "baz"]
      `shouldBe` Just (MkPattern ["foo", "bar", "baz"] Nothing)
    matchSignature pat (Sym "xs")
      `shouldBe` Just (MkPattern [] (Just "xs"))
    matchSignature pat (Num 1)
      `shouldBe` Nothing
    matchSignature numOrStr (Num 1)
      `shouldBe` Just (Left 1)
    matchSignature numOrStr (Str "foo")
      `shouldBe` Just (Right "foo")
    matchSignature numOrStr (Sym "foo")
      `shouldBe` Nothing

argsSignatureTest : IO ()
argsSignatureTest = describe "ArgsSignature" $ do
  let zero      : ArgsSignature () = []
  let one       : ArgsSignature () = [Any "x"]
  let two       : ArgsSignature () = [Any "x", Any "y"]
  let twoOrMore : ArgsSignature () = (Any "x" :: Any "y" :: Rest (Any "z"))

  describe "show" $ do
    zero `shouldShow` "()"
    one `shouldShow` "(x)"
    two `shouldShow` "(x y)"
    twoOrMore `shouldShow` "(x y z ...)"
  describe "match" $ do
    matchArgsSignature zero []
      `shouldBe` Just ()
    matchArgsSignature zero [Num 1]
      `shouldBe` Nothing
    matchArgsSignature one [Num 1]
      `shouldBe` Just (Num 1)
    matchArgsSignature one []
      `shouldBe` Nothing
    matchArgsSignature one [Num 1, Num 2]
      `shouldBe` Nothing
    matchArgsSignature two [Num 1, Num 2]
      `shouldBe` Just (Num 1, Num 2)
    matchArgsSignature two [Num 1, Num 2, Num 3]
      `shouldBe` Nothing
    matchArgsSignature twoOrMore [Num 1, Num 2]
      `shouldBe` Just (Num 1, Num 2, [])
    matchArgsSignature twoOrMore [Num 1]
      `shouldBe` Nothing
    matchArgsSignature twoOrMore [Num 1, Num 2, Num 3, Num 4]
      `shouldBe` Just (Num 1, Num 2, [Num 3, Num 4])

export
test : IO ()
test = describe "Idrlisp.Signature" $ do
  signatureTest
  argsSignatureTest

