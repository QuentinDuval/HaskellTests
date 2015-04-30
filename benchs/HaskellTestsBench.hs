module Main where


import Criterion.Main
import MemoizationBench
import System.IO


main :: IO()
main = do
   hSetEncoding stdout utf8 
   defaultMain [ memoizationBenchs ]

