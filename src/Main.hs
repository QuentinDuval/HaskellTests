module Main where

import Performance.Memoization
import Performance.IOPartialSorting


main::IO()
main = do
   --let res = withMemoTree {-10000000-} 12793129379123
   --print res
   
   --testSlow -- 5.36s (354 MB)
   --testByteString -- 3.5s (307 MB)
   testConduit -- 6.61s (3MB)
