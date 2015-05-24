module Fibo where


import Control.Monad
import Control.Monad.Cont
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.List
import Data.STRef




fib :: Int -> Integer
fib 0 = 0
fib n = snd $ foldl' fibImpl (0,1) [2..n]
   where
      fibImpl (a, b) _ = (b, b + a)


fibIter :: Int -> Integer
fibIter 0 = 0
fibIter n = map snd (iterate fibImpl (0,1)) !! (n-1)
   where
      fibImpl (a, b) = (b, b + a)


fibState :: Int -> Integer
fibState 0 = 0
fibState n = snd $ execState (replicateM (n-1) fibImpl) (0,1)
   where
      fibImpl = modify $ \(a, b) -> (b, b + a)


fibCont :: Int -> Integer
fibCont 0 = 0 
fibCont n = runCont (fibImpl (n-1) (0,1)) snd
   where
      fibImpl 0 (a, b) = return (a, b)
      fibImpl k (a, b) = fibImpl (k-1) (b, b + a)


fibST :: Int -> Integer
fibST n = runST $ do
   a <- newSTRef (0 :: Integer)
   b <- newSTRef (1 :: Integer)
   replicateM_ (n - 1) $ do
      a' <- readSTRef a
      writeSTRef a =<< readSTRef b
      modifySTRef' b (+ a')
   readSTRef b




