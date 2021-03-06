{-# LANGUAGE EmptyDataDecls #-}
module Composition.Types where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Traversable


-- | Dummy data types

data A
data A'
data B
data B'
data C


-- | Simple function composition

f1 :: B -> C
f1 = undefined

g1 :: A -> B
g1 = undefined

h1 :: A -> C
h1 = f1 . g1


-- | "After" function takes 2 arguments

f2 :: B -> B' -> C
f2 = undefined

h21 :: A -> B' -> C
h21 = f2 . g1


-- | "After" function takes 2 arguments

f1' :: (A' -> B) -> C
f1' = undefined

g2 :: A -> A' -> B
g2 = undefined

h12 :: A -> C
h12 = f1' . g2


-- | Both functions take 2 arguments

f2' :: (A' -> B) -> B' -> C
f2' = undefined

h22 :: A -> B' -> C
h22 = f2' . g2


-- | Function composition's composition

compR :: (B -> C) -> (A -> B) -> (A -> C)
compR = (.)

compL :: ((A -> B) -> (A -> C)) -> (a -> (A -> B)) -> a -> (A -> C)
compL = (.)

hcomp :: (B -> C) -> (a -> A -> B) -> (a -> A -> C)
hcomp = compL . compR


-- | Fmap dot Fmap

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

fold2 :: (Foldable f, Foldable g, Monoid m) => (a -> m) -> f (g a) -> m
fold2 = foldMap . foldMap

traverse2 ::
   (Traversable t, Traversable u, Applicative m) =>
   (a -> m b) -> t (u a) -> m (t (u b))
traverse2 = traverse . traverse


-- | Examples

(...) :: (b -> c) -> (a1 -> a -> b) -> a1 -> a -> c
(...) = (.) . (.)

(>->) :: (a1 -> a -> b) -> (b -> c) -> a1 -> a -> c
(>->) = flip (...)

compTest :: IO()
compTest = do
   print $ ((*2) ... (+)) 1 2
   print $ ((+) >-> (*2)) 1 2
   print $ (fmap . fmap) (+1) [[1, 2], [3, 4]]
   print $ (foldMap . foldMap) Sum [[1, 2], [3, 4]]
   void $ (traverse . traverse) print [[1, 2], [3, 4]]

