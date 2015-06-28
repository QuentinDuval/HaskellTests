module ListMonadTests where

import MonadTry.ListMonad

import Test.Tasty
import Test.Tasty.HUnit


listMonadTest :: TestTree
listMonadTest =
   testCase "" $ do
      assertEqual "8" 92 (length $ eightQueens 8)
      assertEqual "8" 92 (length $ eightQueens' 8)

