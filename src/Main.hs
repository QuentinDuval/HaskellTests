module Main where

import Performance.Memoization
import Performance.IOPartialSorting


main::IO()
main = do
   --let res = withMemoTree {-10000000-} 12793129379123
   --print res
   
   -- testSlow -- Takes a whole lot of memory
   testQuick -- Not quick ! But takes much less memory.
