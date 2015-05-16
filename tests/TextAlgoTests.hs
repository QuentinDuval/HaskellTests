{-# LANGUAGE OverloadedStrings #-}
module TextAlgoTests where

import Test.Tasty
import Test.Tasty.HUnit
import TextAlgo

dijkstraTwoStackTest :: TestTree
dijkstraTwoStackTest = testCase "" $ do
   assertEqual "withParams" (Right 30)             (dijkstraTwoStack "((2 1 +) 3 *) 21 +")
   assertEqual "withParams" (Right 30)             (dijkstraTwoStack "((2 1 +) 3 /) 30 *")
   assertEqual "noParams"   (Right 30)             (dijkstraTwoStack "2 1 + 3 * 21 +")
   assertEqual "stickyOps"  (Right 30)             (dijkstraTwoStack "2 1+3 *21+")
   assertEqual "missingOps" (Left MissingOperator) (dijkstraTwoStack "((2 1 +) 3) 21 +")
   assertEqual "unknownTok" (Left UnknownToken)    (dijkstraTwoStack "((2 1 +) 3a) 21 +")
