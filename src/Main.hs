module Main where

import Memoization


main::IO()
main = do
   let res = withMemoTree {-10000000-} 1000000000000000000000000000
   print res
