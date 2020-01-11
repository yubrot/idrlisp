module Test.Env

import Test.Assertions
import Idrlisp.Monad
import Idrlisp.Env as Env

export
test : IO ()
test = describe "Idrlisp.Env" $ do
  describe "single" $ do
    env <- Env.new' Nothing
    runLIO (Env.lookup "foo" env)
      `shouldBe'` Right Nothing
    runLIO (Env.get "foo" env)
      `shouldSatisfy'` isLeft
    runLIO (Env.set "foo" "value" env)
      `shouldSatisfy'` isLeft
    runLIO (Env.define "foo" "value" env)
      `shouldSatisfy'` isRight
    runLIO (Env.lookup "foo" env)
      `shouldBe'` Right (Just "value")
    runLIO (Env.set "foo" "new-value" env)
      `shouldSatisfy'` isRight
    runLIO (Env.lookup "foo" env)
      `shouldBe'` Right (Just "new-value")
    runLIO (Env.get "foo" env)
      `shouldBe'` Right "new-value"
  describe "chained" $ do
    parent <- Env.new' Nothing
    child <- Env.new' (Just parent)
    runLIO (Env.define "foo" "value" parent)
      `shouldSatisfy'` isRight
    runLIO (Env.lookup "foo" child)
      `shouldBe'` Right (Just "value")
    runLIO (Env.get "foo" child)
      `shouldBe'` Right "value"
    runLIO (Env.set "foo" "new-value" child)
      `shouldSatisfy'` isRight
    runLIO (Env.lookup "foo" child)
      `shouldBe'` Right (Just "new-value")
    runLIO (Env.lookup "foo" parent)
      `shouldBe'` Right (Just "new-value")
    runLIO (Env.set "bar" "value" child)
      `shouldSatisfy'` isLeft
    runLIO (Env.define "bar" "value" child)
      `shouldSatisfy'` isRight
    runLIO (Env.lookup "bar" child)
      `shouldBe'` Right (Just "value")
    runLIO (Env.lookup "bar" parent)
      `shouldBe'` Right Nothing
    runLIO (Env.set "bar" "new-value" child)
      `shouldSatisfy'` isRight
    runLIO (Env.lookup "bar" child)
      `shouldBe'` Right (Just "new-value")
    runLIO (Env.lookup "bar" parent)
      `shouldBe'` Right Nothing
    runLIO (Env.define "foo" "shadowing" child)
      `shouldSatisfy'` isRight
    runLIO (Env.lookup "foo" child)
      `shouldBe'` Right (Just "shadowing")
    runLIO (Env.lookup "foo" parent)
      `shouldBe'` Right (Just "new-value")
    runLIO (Env.set "foo" "new-shadowing" child)
      `shouldSatisfy'` isRight
    runLIO (Env.lookup "foo" child)
      `shouldBe'` Right (Just "new-shadowing")
    runLIO (Env.lookup "foo" parent)
      `shouldBe'` Right (Just "new-value")

