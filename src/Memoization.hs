{-# LANGUAGE RecordWildCards, MultiWayIf #-}
module Memoization where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.ST
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashTable.ST.Linear as MHM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV



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


noMemoF :: Int -> Int
noMemoF = fix f


withMemoList :: Int -> Int
withMemoList n =
   let xs = f recf <$> [0 .. n]
       recf = genericIndex xs
   in last xs


withMemoMap :: Int -> Int
withMemoMap n = 
   let xs = foldl (\m k -> M.insert k (f recf k) m) M.empty [0..n]
       recf n = fromJust $ M.lookup n xs
   in fromJust $ M.lookup n xs


withMemoHMap :: Int -> Int
withMemoHMap n = 
   let xs = foldl (\m k -> HM.insert k (f recf k) m) HM.empty [0..n]
       recf n = fromJust $ HM.lookup n xs
   in fromJust $ HM.lookup n xs


withMemoMutMap :: Int -> Int
withMemoMutMap n = runST $ do
   ht <- MHM.newSized (n+1)
   let recf n = fromJust <$> MHM.lookup ht n
   forM_ [0..n] $ \i -> MHM.insert ht i =<< fm recf i
   fromJust <$> MHM.lookup ht n


withMemoVect :: Int -> Int
withMemoVect n =
   let xs = f recf <$> V.fromList [0.. n]
       recf = (V.!) xs
   in V.last xs


withMemoMutVect :: Int -> Int
withMemoMutVect n =
   V.last $ V.create $ do
      xs <- MV.new (n + 1)
      let recf = MV.read xs
      forM_ [0..n] $ \i -> MV.write xs i =<< fm recf i
      return xs


-- | Edward Kmett's solution from http://stackoverflow.com/questions/3208258/memoization-in-haskell

data InfiniteTree a = Tree { value :: a, left, right :: InfiniteTree a }

instance Functor InfiniteTree where
   fmap f' Tree{..} = Tree { value = f' value, left = fmap f' left, right = fmap f' right } 

treeIdx :: InfiniteTree a -> Int -> a
treeIdx t 0        = value t
treeIdx Tree{..} n =
   let (q, r) = (n - 1) `divMod` 2
   in if | r == 0 -> treeIdx left q
         | r == 1 -> treeIdx right q 
 
allTreeIndices :: InfiniteTree Int
allTreeIndices = go 0 1 where
   go pos level =
      let level' = 2 * level
      in Tree { value = pos,
                left  = go (pos + level) level',
                right = go (pos + level') level'}

treeToList :: InfiniteTree a -> [a]
treeToList t = treeIdx t <$> [0..]

withMemoTree :: Int -> Int
withMemoTree n =
   let xs = f recf <$> allTreeIndices
       recf = treeIdx xs
   in treeIdx xs n
