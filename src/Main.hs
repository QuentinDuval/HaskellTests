module Main where

import Memoization


main::IO()
main = do
   let res = withMemoTree {-10000000-} 12793129379123
   print res
