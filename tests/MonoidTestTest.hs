{-# LANGUAGE OverloadedStrings #-}

module MonoidTestTest where

import MonoidTest
import Test.Tasty
import Test.Tasty.HUnit


sortPersonTest :: TestTree
sortPersonTest = testCase "" $ do
   assertBool "=" (Person "a" "b" 1 == Person "a" "b" 1)
   assertBool "<" (Person "a" "b" 1 < Person "c" "b" 1)
   assertBool "<" (Person "a" "b" 1 < Person "a" "c" 1)
   assertBool "<" (Person "a" "b" 1 < Person "a" "b" 2)
   



