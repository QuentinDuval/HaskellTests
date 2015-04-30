module Main where

import MemoizationTest
import MonoidTestTest
import MultiwayIfTests

import Test.Tasty


main :: IO()
main = defaultMain $ testGroup "allTests" [sortPersonTest, fizzBuzzTest, memoizationTest]

