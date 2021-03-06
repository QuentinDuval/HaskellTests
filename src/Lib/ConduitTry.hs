{-# LANGUAGE OverloadedStrings #-}
module Lib.ConduitTry where

import Control.Monad
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State(get, put)
import Data.Conduit as C
import Data.Conduit.Binary as CB
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
   let t' = TB.fromText (T.init t) <> " " <> decimal n <> "\n"
   yield $ toStrict $ TB.toLazyText t'


testConduit :: Handle -> Handle -> IO ()
testConduit input output = do
   g <- getStdGen 
   runConduit $
      sourceHandle input $$ CB.lines $= C.map decodeUtf8
      $= addRandNumber g $= concatBoth
      $= C.map encodeUtf8 $= sinkHandle output


test1 :: IO()
test1 = testConduit stdin stdout

test2 :: IO()
test2 =
   withFile "conduitTest.txt" ReadMode $ \input ->
      withFile "conduitTestOut.txt" WriteMode $ \output ->
         testConduit input output


