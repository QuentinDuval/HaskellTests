{-# LANGUAGE OverloadedStrings #-}
module ConduitTry where

import Control.Monad
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State(get, put)
import Data.Conduit as C
import Data.Conduit.Binary(sourceHandle, sinkHandle)
import Data.Conduit.List as C (map)
import Data.Conduit.Lift(evalStateLC)
import Data.Monoid((<>))
import Data.Text as T
import Data.Text.Lazy(toStrict)
import Data.Text.Lazy.Builder as TB
import Data.Text.Lazy.Builder.Int(decimal)
import Data.Text.Encoding(decodeUtf8, encodeUtf8)
import System.IO
import System.Random



-- TODO - Investigate the possibility of parallelism (each stage in a spark)

addRandNumber :: (Monad m, RandomGen g) => g -> Conduit Text m (Text, Int)
addRandNumber g = void $ evalStateLC g $
   awaitForever $ \t -> do
      g' <- lift get
      let (i, g'') = next g'
      yield (t, i)
      lift $ put g''


concatBoth :: (Monad m) => Conduit (Text, Int) m Text
concatBoth = awaitForever $ \(t, n) -> do
   let t' = TB.fromText t <> " " <> decimal n <> "\n"
   yield $ toStrict $ TB.toLazyText t'


testConduit :: Handle -> Handle -> IO ()
testConduit input output = do
   g <- getStdGen 
   runConduit $ sourceHandle input $$ C.map decodeUtf8
      $= addRandNumber g $= concatBoth
      $= C.map encodeUtf8 $= sinkHandle output


testConduit' :: IO ()
testConduit' = testConduit stdin stdout

