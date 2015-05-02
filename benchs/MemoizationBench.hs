{-# LANGUAGE BangPatterns #-}
module MemoizationBench where

import Criterion
import Memoization


memoizationBenchs :: Benchmark
memoizationBenchs =
   let !n = 100000
   in bgroup "memoization"
      [ bench "noMemoF"        $ nf noMemoF n
      , bench "withMemoTree"   $ nf withMemoTree n
      , bench "withMemoList"   $ nf withMemoList n
      , bench "withMemoMap"    $ nf withMemoMap n
      , bench "withMemoHMap"   $ nf withMemoHMap n
      , bench "withMemoMutMap" $ nf withMemoMutMap n
      , bench "withMemoVect"   $ nf withMemoVect n ]
