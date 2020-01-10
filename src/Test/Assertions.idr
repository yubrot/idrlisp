module Test.Assertions

%access export

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

