module FiboBench where

import Criterion.Main
import Fibo


benchFibo :: Benchmark
benchFibo =
   let iter = 2000
   in bgroup "fiboBench" 
      [bench "fib"      $ nf fib iter,
       bench "fibIter"  $ nf fibIter iter,
       bench "fibState" $ nf fibState iter,
       bench "fibCont"  $ nf fibCont iter,
       bench "fibST"    $ nf fibST iter,
       bench "fibST'"   $ nf fibST' iter]
