module Main where

import Performance.Memoization
import Performance.IOPartialSorting


main::IO()
main = do
   -- print $ withMemoTree {-10000000-} 12793129379123
   
   -- testSlow -- 5.16s (354 MB)
   -- testByteString -- 3.53s (307 MB)
   testByteString' -- 0.73s (283 MB)
   -- testParsec -- 0.88s (313 MB)
   -- testConduit -- 1.22s (4MB)
