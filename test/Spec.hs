module Main where

import FizzBuzz

import Test.HUnit
import Test.QuickCheck


-- | Example based tests

testCases :: [(Int, String)]
testCases =
  [(0, "0")
  ,(1, "1")
  ,(3, "Fizz")
  ,(5, "Buzz")
  ,(7, "7")
  ,(15, "FizzBuzz")
  ,(100, "Buzz")]

tests = TestList $ map createTestCase testCases
  where
    createTestCase (input, expected) =
      TestCase (assertEqual ("FizzBuzz of " ++ show input) expected (fizzBuzz input))


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
