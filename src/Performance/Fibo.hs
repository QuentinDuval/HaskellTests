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


fibIter :: Int -> Integer
fibIter n = map fst (iterate fibImpl (0,1)) !! n
   where
      fibImpl (a, b) = (b, b + a)


fibState :: Int -> Integer
fibState n = fst $ execState (replicateM n fibImpl) (0,1)
   where
      fibImpl = modify $ \(a, b) -> (b, b + a)


fibCont :: Int -> Integer
fibCont n = runCont (fibImpl n (0,1)) fst
   where
      fibImpl 0 (a, b) = return (a, b)
      fibImpl k (a, b) = fibImpl (k-1) (b, b + a)


fibST :: Int -> Integer
fibST 0 = 0
fibST n = runST $ do
   a <- newSTRef (0 :: Integer)
   b <- newSTRef (1 :: Integer)
   replicateM_ (n - 1) $ do
      a' <- readSTRef a
      writeSTRef a =<< readSTRef b
      modifySTRef' b (+ a')
   readSTRef b


fibST' :: Int -> Integer
fibST' 0 = 0
fibST' n = runST $ do
   res <- newSTRef (0,1)
   replicateM_ n $
      modifySTRef' res $ \(a, b) -> (b, a + b)
   fst <$> readSTRef res



