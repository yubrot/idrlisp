module Test.Assertions

%access export
%default total

describe : String -> IO () -> IO ()
describe label body = do
  putStrLn ""
  putStr label
  body

assert : Bool -> Lazy (List String) -> IO ()
assert False report = do
  putStrLn "*FAIL*"
  traverse_ (\x => putStrLn ("  " ++ x)) report
assert True report =
  putStr "."

shouldBe : (Eq a, Show a) => a -> a -> IO ()
shouldBe a expected =
  assert (a == expected) [
    "Expected " ++ show expected,
    "But got  " ++ show a
  ]

shouldShow : Show a => a -> String -> IO ()
shouldShow a expected =
  assert (show a == expected) [
    "Expected to show " ++ expected,
    "But got          " ++ show a
  ]

shouldSatisfy : Show a => a -> (a -> Bool) -> IO ()
shouldSatisfy a pred =
  assert (pred a) [
    "Expectation unsatisfied: " ++ show a
  ]

castIdentical : (Cast a b, Cast b a, Eq a, Eq b) => (a, b) -> Bool
castIdentical (a, b) = cast a == b && cast b == a

shouldBe' : (Eq a, Show a) => IO a -> a -> IO ()
shouldBe' a b = a >>= (`shouldBe` b)

shouldShow' : Show a => IO a -> String -> IO ()
shouldShow' a b = a >>= (`shouldShow` b)

shouldSatisfy' : Show a => IO a -> (a -> Bool) -> IO ()
shouldSatisfy' a b = a >>= (`shouldSatisfy` b)

