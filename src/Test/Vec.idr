module Test.Vec

import Test.Assertions
import Idrlisp.Vec

export
test : IO ()
test = describe "Test.Vec" $ do
  describe "empty, new, fromList, toList" $ do
    Vec.toList (the (Vec ()) Vec.empty)
      `shouldBe'` []
    vec <- Vec.new 3 "foo"
    Vec.toList vec
      `shouldBe'` ["foo", "foo", "foo"]
    vec <- Vec.fromList (the (List ()) [])
    Vec.toList vec
      `shouldBe'` []
    vec <- Vec.fromList ["a", "b", "c"]
    Vec.toList vec
      `shouldBe'` ["a", "b", "c"]
  describe "length" $ do
    Vec.length (the (Vec ()) Vec.empty)
      `shouldBe` 0
    vec <- Vec.new 3 "foo"
    Vec.length vec
      `shouldBe` 3
  describe "read" $ do
    vec <- Vec.fromList ["a", "b", "c"]
    Vec.read 0 vec
      `shouldBe'` Just "a"
    Vec.read 1 vec
      `shouldBe'` Just "b"
    Vec.read 2 vec
      `shouldBe'` Just "c"
    Vec.read 3 vec
      `shouldBe'` Nothing
  describe "write" $ do
    vec <- Vec.fromList ["a", "b", "c"]
    Vec.write 0 "x" vec
      `shouldBe'` True
    Vec.write 2 "y" vec
      `shouldBe'` True
    Vec.write 4 "z" vec
      `shouldBe'` False
    Vec.toList vec
      `shouldBe'` ["x", "b", "y"]
  describe "copy" $ do
    src <- Vec.fromList ["a", "b", "c", "d"]
    dest <- Vec.fromList ["1", "2", "3", "4", "5"]
    Vec.copy src 1 dest 2 3
      `shouldBe'` True
    Vec.toList src
      `shouldBe'` ["a", "b", "c", "d"]
    Vec.toList dest
      `shouldBe'` ["1", "2", "b", "c", "d"]

