{-# LANGUAGE ScopedTypeVariables #-}
module MemoizationBench where

import Criterion
import Performance.Memoization


memoizationBenchs :: Benchmark
memoizationBenchs =
   let n = 1000000
   in bgroup "memoization"
      [ bench "noMemoF"        $ nf noMemoF n
      , bench "withMemoList"   $ nf withMemoList n
      , bench "withMemoVect"   $ nf withMemoVect n
      , bench "withMemoStMap"  $ nf withMemoStMap n
      , bench "withMemoStHMap" $ nf withMemoStHMap n
      , bench "withMemoMutMap" $ nf withMemoMutMap n
      , bench "withMemoTree"   $ nf withMemoTree n ]
