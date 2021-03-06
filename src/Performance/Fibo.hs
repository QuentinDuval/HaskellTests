{-# LANGUAGE BangPatterns #-}
module Performance.Fibo where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.List
import Data.STRef



fib :: Int -> Integer
fib n = fst $ foldl' next (0,1) [1..n]
   where
      next (!a, !b) _ = (b, b + a)


fibRec :: Int -> Integer
fibRec = go 0 1
   where
      go !a _  0  = a
      go !a !b !k = go b (a + b) (k - 1)


fibFast :: Int -> Integer
fibFast = fibVal . fibPow
   where
      -- Recurrence relation 
      fibMat = (1, 1,
                1, 0)
      
      -- Multiplication with vector (1, 0)
      fibVal (_, _, c, _) = c
            
      -- Fast exponentiation
      fibPow n
         | n <= 0    = (1, 0, 0, 1) -- Identity matrix
         | odd n     = fibMult fibMat $! fibPow (n - 1)
         | otherwise = fibSq $! fibPow (div n 2)
      
      fibSq m = fibMult m m
      fibMult (a , b , c , d ) (a', b', c', d')
         = ( a * a' + b * c', a * b' + b * d'
           , c * a' + d * c', c * b' + d * d')


fibCont :: Int -> Integer
fibCont n = fst $ go n id
   where
      next (a, b) = (b, a + b)
      go 0 c = c  (0, 1)
      go k c = go (k - 1) (next . c)


fibUnfold :: Int -> Integer
fibUnfold n = fst $ last $ unfoldr go (0, 1, n + 1)
   where
      go (_ ,  _,  0) = Nothing
      go (!a, !b, !k) = Just ((a, b), (b, a + b, k - 1))


fibIter :: Int -> Integer
fibIter n = map fst (iterate next (0,1)) !! n
   where
      next (a, b) = (b, b + a)


fibState :: Int -> Integer
fibState n = fst $ execState (replicateM n next) (0,1)
   where
      next = modify $ \(a, b) -> (b, b + a)


fibST :: Int -> Integer
fibST 0 = 0
fibST n = runST $ do
   a <- newSTRef 0
   b <- newSTRef 1
   replicateM_ (n - 1) $! do
      !a' <- readSTRef a
      !b' <- readSTRef b
      writeSTRef a b'
      modifySTRef' b (+ a')
   readSTRef b


