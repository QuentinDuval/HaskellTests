{-# LANGUAGE OverloadedStrings #-}
module ConduitTry where

import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import Data.Text
import System.IO
import System.Random




hSource :: (MonadIO m) => Handle -> Source m Text 
hSource h = addCleanup (const $ liftIO $ hClose h) loop
   where
      loop = do
         eof <- liftIO $ hIsEOF h
         unless eof $ do
            str <- liftIO $ hGetLine h
            yield (pack str)
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


hSink :: (MonadIO m) => Handle -> Sink (Text, Int) m ()
hSink out = awaitForever $ \(t, n) ->
   liftIO $ hPutStrLn out (unpack t ++ " " ++ show n) 


testConduit :: Handle -> Handle -> IO ()
testConduit input output = do
   g <- getStdGen 
   runConduit (hSource input $$ addRandNumber g $= hSink output)


testConduit' :: IO ()
testConduit' = testConduit stdin stdout

