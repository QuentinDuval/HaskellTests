{-# LANGUAGE BangPatterns #-}
module Performance.Fibo where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.List
import Data.STRef



fib :: Int -> Integer
fib n = fst $ foldl' fibImpl (0,1) [1..n]
   where
      fibImpl (a, b) _ = (b, b + a)


fibRec :: Int -> Integer
fibRec n = go n 0 1
   where
      go 0  !a _  = a
      go !k !a !b = go (k - 1) b (a + b)


fibCont :: Int -> Integer
fibCont n = runCont (go n) snd
   where
      go 1 = return (0, 1)
      go k = do
         (!a, !b) <- go (k - 1)
         return (b, a + b)


fibIter :: Int -> Integer
fibIter n = map fst (iterate fibImpl (0,1)) !! n
   where
      fibImpl (a, b) = (b, b + a)


fibState :: Int -> Integer
fibState n = fst $ execState (replicateM n fibImpl) (0,1)
   where
      fibImpl = modify $ \(a, b) -> (b, b + a)


fibST :: Int -> Integer
fibST 0 = 0
fibST n = runST $ do
   a <- newSTRef 0
   b <- newSTRef 1
   replicateM_ (n - 1) $! do
      a' <- readSTRef a
      b' <- readSTRef b
      modifySTRef' a (const b')
      modifySTRef' b (+ a')
   readSTRef b


fibST' :: Int -> Integer
fibST' 0 = 0
fibST' n = runST $ do
   res <- newSTRef (0,1)
   replicateM_ n $
      modifySTRef' res $ \(a, b) -> (b, a + b)
   fst <$> readSTRef res






