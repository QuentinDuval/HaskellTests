module Main where

import FiboTests
import MemoizationTest
import MonoidTestTest
import MultiwayIfTests
import TextAlgoTests

import Test.Tasty


main :: IO()
main = defaultMain $ testGroup "allTests"
   [sortPersonTest, fizzBuzzTest, memoizationTest, dijkstraTwoStackTest, fiboTest]

