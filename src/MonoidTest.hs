{-# LANGUAGE OverloadedStrings #-}

module MonoidTest where

import Data.Foldable
import Data.Function
import Data.Text


data Person = SortableTest {
   _lastName   :: Text,
   _firstName  :: Text,
   _age        :: Int
}

instance Eq Person where
   lhs == rhs = EQ == compare' lhs rhs

instance Ord Person where
   compare = compare'


compare' :: Person -> Person -> Ordering
compare' lhs rhs =
   let comps = [compare `on` _lastName, compare `on` _firstName, compare `on` _age]
   in foldMap (\f -> f lhs rhs) comps -- Makes use of the Ordering Monoid


