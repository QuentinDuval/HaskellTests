module MemoizationTest where

import Control.Monad
import Memoization

import Test.Tasty
import Test.Tasty.HUnit


memoizationTest :: TestTree
memoizationTest = testCase "" $ do
   let toTest = [noMemoF, withMemoTree, withMemoList,
                 withMemoMap, withMemoHMap, withMemoMutMap,
                 withMemoVect, withMemoMutVect]
   forM_ toTest $ \f' -> assertBool "=" (203708 == f' 100000)


