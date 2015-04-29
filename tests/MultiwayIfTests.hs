{-# LANGUAGE OverloadedStrings #-}

module MultiwayIfTests where

import MultiwayIf
import Test.Tasty
import Test.Tasty.HUnit

fizzBuzzTest :: TestTree
fizzBuzzTest = testCase "" $ do
   assertEqual "simple"  ["1", "2", "Fizz", "4", "Buzz"] (take 5 fizzBuzz)
   assertEqual "complex" "FizzBuzz" (fizzBuzz !! 14)
