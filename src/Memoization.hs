module Memoization where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
-- import Data.Function(fix)
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV



f :: (Integral a, Monad m) => (a -> m a) -> a -> m a
f _    0 = return 0
f recf n = do
   recs <- mapM recf $ (n `div`) <$> [2, 3, 4]
   return $ max n (sum recs)


noMemoF :: Int -> Int
noMemoF n = runIdentity $ fix f n


withMemoL :: Int -> Int
withMemoL n = runIdentity $
   let xs = f recf <$> [0 .. n]
       recf = genericIndex xs
   in last xs


withMemoV :: Int -> Int
withMemoV n = runIdentity $
   let xs = f recf <$> V.fromList [0.. n]
       recf = (V.!) xs
   in V.last xs


withMemoMV :: Int -> Int
withMemoMV n =
   V.last $ V.create $ do
      xs <- MV.new (n + 1)
      let recf = MV.read xs
      forM_ [0..n] $ \i -> MV.write xs i =<< f recf i
      return xs


