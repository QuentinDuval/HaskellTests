module Main where

import Memoization


main::IO()
main = do
   let res = noMemoF 10000000
   print res
