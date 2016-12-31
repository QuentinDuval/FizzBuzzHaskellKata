module Main where

import FizzBuzz

import Test.HUnit
import Test.QuickCheck


-- | Example based tests

tests :: Test
tests =
  let multiples_of_3_and_5 = [3 * 5, 2 * 3 * 5, 3 * 3 * 5]
      multiples_of_3 = [3, 1 * 3, 2 * 3, 3 * 3]
      multiples_of_5 = [5, 1 * 5, 2 * 5, 3 * 5]
      other_numbers = [0, 1, 2, 4]
  in TestList
    [ fizzBuzz `should_equal` "FizzBuzz" `for_all` multiples_of_3_and_5
    , fizzBuzz `should_equal` "Fizz" `for_all` multiples_of_3
    , fizzBuzz `should_equal` "Buzz" `for_all` multiples_of_5
    , fizzBuzz `should_do_like` show `for_all` other_numbers
    ]

should_equal :: (Show t, Show a, Eq a) => (t -> a) -> a -> t -> Test
should_equal underTest expected input =
  let label = "Expects " ++ show expected ++ " for " ++ show input
  in TestCase $ assertEqual label expected (underTest input)

should_do_like :: (Show t, Show a, Eq a) => (t -> a) -> (t -> a) -> t -> Test
should_do_like underTest refFct input
  = should_equal underTest (refFct input) input

for_all :: (a -> Test) -> [a] -> Test
for_all fct inputs = TestList (map fct inputs)


-- | QuickCheck fun

newtype Fizz = Fizz Int deriving (Show)
newtype Buzz = Buzz Int deriving (Show)
newtype Other = Other Int deriving (Show)

instance Arbitrary Fizz where
  arbitrary = do
    x <- (arbitrary `suchThat` (\n -> fizzBuzz n == "Fizz"))
    return (Fizz x)

instance Arbitrary Buzz where
  arbitrary = do
    x <- (arbitrary `suchThat` (\n -> fizzBuzz n == "Buzz"))
    return (Buzz x)

instance Arbitrary Other where
  arbitrary = do
    x <- (arbitrary `suchThat` (\n -> fizzBuzz n == (show n) && n /= 0))
    return (Other x)

prop_fizzBuzz :: Fizz -> Buzz -> Bool
prop_fizzBuzz (Fizz a) (Buzz b) = fizzBuzz (a * b) == "FizzBuzz"

prop_fizzFizz :: Fizz -> Fizz -> Bool
prop_fizzFizz (Fizz a) (Fizz b) = fizzBuzz (a * b) == "Fizz"

prop_buzzBuzz :: Buzz -> Buzz -> Bool
prop_buzzBuzz (Buzz a) (Buzz b) = fizzBuzz (a * b) == "Buzz"

prop_fizz :: Other -> Fizz -> Bool
prop_fizz (Other a) (Fizz b) = fizzBuzz (a * b) == "Fizz"

prop_buzz :: Other -> Buzz -> Bool
prop_buzz (Other a) (Buzz b) = fizzBuzz (a * b) == "Buzz"


-- Test launcher

main :: IO ()
main = do
  runTestTT tests
  quickCheck prop_fizzBuzz
  quickCheck prop_buzz
  quickCheck prop_fizz
  quickCheck prop_fizzFizz
  quickCheck prop_buzzBuzz
