module MemoizationBench where

import Criterion
import Memoization


memoizationBenchs :: Benchmark
memoizationBenchs = bgroup "memoization"
   [ bench "noMemoF"      $ nf noMemoF 100000
   , bench "withMemoL"    $ nf withMemoL 100000
   , bench "withMemoV"    $ nf withMemoV 100000
   , bench "withMemoMV"   $ nf withMemoMV 100000 ]
