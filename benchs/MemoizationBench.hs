module MemoizationBench where

import Criterion
import Criterion.Main
import Memoization


main :: IO ()
main = defaultMain [
      bench "noMemoF"      $ nf noMemoF 100000,
      bench "withMemoL"    $ nf withMemoL 100000,
      bench "withMemoV"    $ nf withMemoV 100000,
      bench "withMemoMV"   $ nf withMemoMV 100000
   ]
