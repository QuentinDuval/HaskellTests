module MemoizationBench where

import Criterion
import Memoization


memoizationBenchs :: Benchmark
memoizationBenchs = bgroup "memoization"
   [ bench "noMemoF"        $ nf noMemoF 100000
   , bench "withMemoTree"   $ nf withMemoTree 100000
   , bench "withMemoList"   $ nf withMemoList 100000
   , bench "withMemoMap"    $ nf withMemoMap 100000
   , bench "withMemoHMap"   $ nf withMemoHMap 100000
   , bench "withMemoMutMap" $ nf withMemoMutMap 100000
   , bench "withMemoVect"   $ nf withMemoVect 100000
   , bench "withMemoMVect"  $ nf withMemoMutVect 100000 ]
