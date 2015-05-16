{-# LANGUAGE ScopedTypeVariables #-}
module MemoizationBench where

import Criterion
import Memoization


memoizationBenchs :: Benchmark
memoizationBenchs =
   let n = 1000000
   in bgroup "memoization"
      [ bench "noMemoF"        $ nf noMemoF n
      , bench "withMemoTree"   $ nf withMemoTree n
      , bench "withMemoList"   $ nf withMemoList n
      , bench "withMemoMutMap" $ nf withMemoMutMap n
      , bench "withMemoStMap"  $ nf withMemoStMap n
      , bench "withMemoStHMap" $ nf withMemoStHMap n
      , bench "withMemoVect"   $ nf withMemoVect n ]
