module MemoizationBench where

import Criterion
import Memoization


memoizationBenchs :: Benchmark
memoizationBenchs = bgroup "memoization"
   [ bench "noMemoF"      $ whnf noMemoF 100000
   , bench "withMemoL"    $ whnf withMemoL 100000
   , bench "withMemoV"    $ whnf withMemoV 100000
   , bench "withMemoMV"   $ whnf withMemoMV 100000 ]
