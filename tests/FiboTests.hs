module FiboTests where

import Performance.Fibo

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit


fiboTest :: TestTree
fiboTest = testCase "" $ do
   let toTest = [fib, fibIter, fibState, fibCont, fibST, fibST']
   forM_ toTest $ \f -> do
      assertBool "fib 0"  (0 == f 0)
      assertBool "fib 1"  (1 == f 1)
      assertBool "fib 2"  (1 == f 2)
      assertBool "fib 5"  (5 == f 5)
      assertBool "fib 10" (55 == f 10)
      assertBool "fib 20" (6765 == f 20)
