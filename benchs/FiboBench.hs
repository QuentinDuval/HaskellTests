module FiboBench where

import Criterion.Main
import Performance.Fibo


benchFibo :: Benchmark
benchFibo =
   let iter = 4000
   in bgroup "fibo (of 4000)" 
      [bench "fib"         $ nf fib iter,
       bench "fibRec"      $ nf fibRec iter,
       bench "fibUnfold"   $ nf fibUnfold iter,
       bench "fibIter"     $ nf fibIter iter,
       bench "fibState"    $ nf fibState iter,
       bench "fibCont"     $ nf fibCont iter,
       bench "fibST"       $ nf fibST iter,
       bench "fibFast"     $ nf fibFast iter]
