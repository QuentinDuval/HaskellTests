module Main where
import MonoidTestTest
import MultiwayIfTests

import Test.Tasty


main :: IO()
main = defaultMain $ testGroup "allTests" [sortPersonTest, fizzBuzzTest]

