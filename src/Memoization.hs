{-# LANGUAGE RecordWildCards, MultiWayIf, ScopedTypeVariables #-}
module Memoization where

import Control.Applicative
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Hashable
import qualified Data.Map.Strict as MS
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashTable.ST.Linear as MHM
import qualified Data.Vector as V



f :: (Integral a) => (a -> a) -> a -> a
f _    0 = 0
f recf n =
   let recs = recf <$> div n <$> [2, 3, 4]
   in max n (sum recs)

fm :: (Integral a, Monad m) => (a -> m a) -> a -> m a
fm _    0 = return 0
fm recf n = do
   recs <- mapM recf $ div n <$> [2, 3, 4]
   return $ max n (sum recs)


-- | No memoization of any kind

noMemoF :: (Integral n) => n -> n
noMemoF = f noMemoF -- or equivalently: fix f


-- | Simple lazy data structures

withMemoList :: Int -> Int
withMemoList n =
   let xs = [f recf i | i <- [0..n]]
       recf = (!!) xs
   in last xs

withMemoVect :: Int -> Int
withMemoVect n =
   let xs = V.fromList [f recf i | i <- [0..n]]
       recf = (V.!) xs
   in V.last xs


-- | Attempts with state monad and immutable associative containers

withMemoStMap :: (Integral n) => n -> n
withMemoStMap n = evalState (fm recF n) MS.empty
   where
      recF i = do
         v <- MS.lookup i <$> get
         case v of
            Just v' -> return v' 
            Nothing -> do
               v' <- fm recF i
               modify $ MS.insert i v'
               return v'
 
 
withMemoStHMap :: (Integral n, Hashable n) => n -> n
withMemoStHMap n = evalState (fm recF n) HMS.empty
   where
      recF i = do
         v <- HMS.lookup i <$> get
         case v of
            Just v' -> return v' 
            Nothing -> do
               v' <- fm recF i
               modify $ HMS.insert i v'
               return v'
 

-- | Attempts with mutable data structures

withMemoMutMap :: (Integral n, Hashable n) => n -> n
withMemoMutMap n = runST $
   do ht <- MHM.new
      recF ht n
   where
      recF ht i = do
         k <- MHM.lookup ht i
         case k of
            Just k' -> return k'
            Nothing -> do 
               k' <- fm (recF ht) i
               MHM.insert ht i k'
               return k'


-- | Edward Kmett's solution from http://stackoverflow.com/questions/3208258/memoization-in-haskell

data InfiniteTree a = Tree { value :: a, left, right :: InfiniteTree a }

instance Functor InfiniteTree where
   fmap f' Tree{..} = Tree { value = f' value, left = fmap f' left, right = fmap f' right } 

treeIdx :: (Integral n) => InfiniteTree a -> n -> a
treeIdx t 0        = value t
treeIdx Tree{..} n =
   let (q, r) = (n - 1) `divMod` 2
   in if | r == 0 -> treeIdx left q
         | r == 1 -> treeIdx right q 
 
allTreeIndices :: (Integral n) => InfiniteTree n
allTreeIndices = go 0 1 where
   go pos level =
      let level' = 2 * level
      in Tree { value = pos,
                left  = go (pos + level) level',
                right = go (pos + level') level'}

treeToList :: InfiniteTree a -> [a]
treeToList t = treeIdx t <$> [0..]

withMemoTree :: (Integral n) => n -> n
withMemoTree n =
   let xs = f recf <$> allTreeIndices
       recf = treeIdx xs
   in treeIdx xs n
