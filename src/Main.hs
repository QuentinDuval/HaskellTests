module Main where

import Performance.Memoization
import Performance.IOPartialSorting


main::IO()
main = do
   --let res = withMemoTree {-10000000-} 12793129379123
   --print res
   
   -- testSlow -- 6.30s (354 MB)
   -- testByteString -- 4.41s (307 MB)
   testConduit -- 3.81s (4MB)
   -- testParsec -- 1.45s (319 MB)
