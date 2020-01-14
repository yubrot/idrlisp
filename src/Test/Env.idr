module Test.Env

import Test.Assertions
import Idrlisp.Env as Env

export
test : IO ()
test = describe "Test.Env" $ do
  describe "single" $ do
    env <- Env.new Nothing
    Env.lookup "foo" env `shouldBe'` Left "foo"
    Env.set "foo" "value" env `shouldSatisfy'` isLeft
    Env.define "foo" "value" env
    Env.lookup "foo" env `shouldBe'` Right "value"
    Env.set "foo" "new-value" env `shouldSatisfy'` isRight
    Env.lookup "foo" env `shouldBe'` Right "new-value"
  describe "chained" $ do
    parent <- Env.new Nothing
    child <- Env.new (Just parent)
    Env.define "foo" "value" parent
    Env.lookup "foo" child `shouldBe'` Right "value"
    Env.set "foo" "new-value" child `shouldSatisfy'` isRight
    Env.lookup "foo" child `shouldBe'` Right "new-value"
    Env.lookup "foo" parent `shouldBe'` Right "new-value"
    Env.set "bar" "value" child `shouldSatisfy'` isLeft
    Env.define "bar" "value" child
    Env.lookup "bar" child `shouldBe'` Right "value"
    Env.lookup "bar" parent `shouldBe'` Left "bar"
    Env.set "bar" "new-value" child `shouldSatisfy'` isRight
    Env.lookup "bar" child `shouldBe'` Right "new-value"
    Env.lookup "bar" parent `shouldBe'` Left "bar"
    Env.define "foo" "shadowing" child
    Env.lookup "foo" child `shouldBe'` Right "shadowing"
    Env.lookup "foo" parent `shouldBe'` Right "new-value"
    Env.set "foo" "new-shadowing" child `shouldSatisfy'` isRight
    Env.lookup "foo" child `shouldBe'` Right "new-shadowing"
    Env.lookup "foo" parent `shouldBe'` Right "new-value"

