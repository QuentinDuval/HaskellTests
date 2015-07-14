{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Performance.IOPartialSorting where

import Control.Applicative

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State(get, put)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB

import Data.Conduit as C
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as C
import Data.Conduit.Lift

import Data.Heap(MinPrioHeap)
import qualified Data.Heap as H

import Data.List as L
import Data.Monoid
import Data.Ord

import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding

import Data.Tuple(swap)

import System.IO



-- | Trying to answer:
-- | http://stackoverflow.com/questions/31348236/most-efficient-data-structure-for-finding-most-frequent-items

testSlow :: IO ()
testSlow = do
   inputs <- lines <$> readFile "IOPartialSorting.txt"
   let dataSet = (\[x,y] -> (x, read y :: Int)) <$> (take 2 . words) <$> inputs
   let res = take 5000 $ sortBy (flip $ comparing snd) dataSet
   print res




-- | Trying to get faster with conduit

testQuick :: IO ()
testQuick = withFile "IOPartialSorting.txt" ReadMode $ \h -> do
      res <- runConduit $
               B.sourceHandle h $$ B.lines
               =$ C.map decodeUtf8
               =$ C.map (take 2 . T.words)
               =$ C.map (\[x,y] -> (x, read (T.unpack y) :: Int))
               =$ computeRes 5000
      print res
      
      where
         computeRes :: (Monad m) => Int -> Sink (Text, Int) m [(Text, Int)]
         computeRes nb = do
            let initState = H.empty :: MinPrioHeap Int Text
            r <- execStateLC initState $ awaitForever $ \pair -> do
                     heap <- lift get
                     let !toInsert = swap pair
                     if H.size heap < nb
                        then lift $ put $ H.insert toInsert heap
                        else do
                           let (Just (h, t)) = H.view heap
                           if h > toInsert
                              then return()
                              else lift $ put $ H.insert toInsert t
                     
            return $ reverse $ fmap swap $ H.take nb r
            


-- | To fill an input file

fillInput :: IO ()
fillInput =
   do let filler = C.sourceList [1..1000000]
                     $$ C.map (\c -> ("aaaa", c))
                     =$ C.map (\(t, c) -> t <> " " <> BB.intDec c <> "\n")
                     =$ C.map (BB.toLazyByteString)
                     =$ C.map (BL.toStrict)
                     =$ B.sinkFile "IOPartialSorting.txt" 
      runResourceT $ filler
