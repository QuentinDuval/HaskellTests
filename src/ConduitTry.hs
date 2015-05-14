{-# LANGUAGE OverloadedStrings #-}
module ConduitTry where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString as B
import Data.Conduit as C
import Data.Conduit.Binary
import Data.Conduit.List as C
import Data.Monoid
import Data.Text as T
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder as TB
import Data.Text.Lazy.Builder.Int as TB
import Data.Text.Encoding as E
import System.IO as S
import System.Random



-- TODO - Investigate the possibility of parallelism (each stage in a spark)

-- Equivalent of sourceHandle
hSource :: (MonadIO m) => Handle -> Source m ByteString 
hSource h = addCleanup (const $ liftIO $ hClose h) loop
   where
      loop = do
         eof <- liftIO $ hIsEOF h
         unless eof $ do
            str <- liftIO $ B.hGetLine h
            yield str
            loop


addRandNumber :: (Monad m, RandomGen g) => g -> Conduit Text m (Text, Int)
addRandNumber = loop where
   loop g = do
      v <- await
      case v of
         Nothing -> return ()
         Just t  -> do
            let (i, g') = next g
            yield (t, i)
            loop g'


concatBoth :: (Monad m) => Conduit (Text, Int) m Text
concatBoth = awaitForever $ \(t, n) ->
   yield $ T.toStrict $ TB.toLazyText (TB.fromText t <> " " <> decimal n)


hSink :: (MonadIO m) => Handle -> Sink Text m ()
hSink out = awaitForever $ liftIO . B.hPutStrLn out . E.encodeUtf8


testConduit :: Handle -> Handle -> IO ()
testConduit input output = do
   g <- getStdGen 
   runConduit $
      {- hSource input -} sourceHandle input $$ C.map E.decodeUtf8
      $= addRandNumber g $= concatBoth $= hSink output


testConduit' :: IO ()
testConduit' = testConduit stdin stdout

