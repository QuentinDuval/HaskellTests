module Main where


import FiboBench
import MemoizationBench

import Criterion.Main
import System.IO


main :: IO()
main = do
   hSetEncoding stdout utf8 
   defaultMain [ benchFibo
               {-, memoizationBenchs-} ]

