{-# LANGUAGE TemplateHaskell #-}
module FizzBuzz where

import Test.HUnit
import Test.QuickCheck


fizzBuzz :: Int -> String
fizzBuzz 0 = "0"
fizzBuzz n =
  let res = fizzBuzzImpl [newRule 3 "Fizz", newRule 5 "Buzz"] n
  in if res == ""
      then show n
      else res

type Rule = Int -> String

newRule :: Int -> String -> Rule
newRule divisor out n
  | isMultiple n divisor = out
  | otherwise = "" -- Try with a Maybe (return a Nothing then bind to avoid the last if)

fizzBuzzImpl :: [Rule] -> Int -> String
fizzBuzzImpl rules n = concatMap ($ n) rules

isMultiple :: Int -> Int -> Bool
isMultiple n divisor = mod n divisor == 0
