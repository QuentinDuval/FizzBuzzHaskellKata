module Main where

import FizzBuzz

main :: IO ()
main = mapM_ (print . fizzBuzz) [0..100]
