module HaskellTestsBench where


import Criterion.Main
import MemoizationBench


main :: IO()
main = defaultMain [bgroup "memoization" memoizationBenchs]

