{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Performance.IOPartialSorting where

import Control.Applicative
import Control.Arrow

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State(modify')

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB

import Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Lift

import Data.Heap(MinPrioHeap)
import qualified Data.Heap as H

import Data.List as L
import Data.List.Split(chunksOf)
import Data.Monoid
import Data.Ord

import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding

import Data.Tuple(swap)
import Data.Word8

import System.IO



-- | Trying to answer:
-- | http://stackoverflow.com/questions/31348236/most-efficient-data-structure-for-finding-most-frequent-items

testSlow :: IO ()
testSlow = do
   inputs <- lines <$> readFile "IOPartialSorting.txt"
   let dataSet = (\[x,y] -> (x, read y :: Int)) <$> (take 2 . words) <$> inputs
   let res = take 5000 $ sortBy (flip $ comparing snd) dataSet
   print res


testByteString :: IO ()
testByteString = do
   inputs <- chunksOf 2 <$> B.splitWith isSpace <$> B.readFile "IOPartialSorting.txt"
   let decoded = fmap (fmap decodeUtf8) inputs
   let dataSet = (\[x,y] -> (x, read (T.unpack y) :: Int)) <$> decoded
   let res = take 5000 $ sortBy (flip $ comparing snd) dataSet
   print res


-- | Trying to get faster with conduit

testConduit :: IO ()
testConduit = withFile "IOPartialSorting.txt" ReadMode $ \h -> do
      res <- runConduit $
               CB.sourceHandle h
               $$ CB.lines
               =$ CL.map decodeUtf8
               =$ CL.map (T.breakOn " ")
               =$ CL.map (second ((read :: String -> Int) . T.unpack))
               =$ computeRes 5000
      print res
      
      where
         addToHeap nb pair heap =
            let !toInsert = swap pair
                Just (h, t) = H.view heap
            in if H.size heap < nb
               then H.insert toInsert heap
               else if h > toInsert
                  then heap
                  else H.insert toInsert t
      
         computeRes :: (Monad m) => Int -> Sink (Text, Int) m [(Text, Int)]
         computeRes nb = do
            let initState = H.empty :: MinPrioHeap Int Text
            r <- execStateLC initState $ awaitForever $ lift . modify' . addToHeap nb
            return $ reverse $ swap <$> H.take nb r



-- | To fill an input file

fillInput :: IO ()
fillInput =
   do let filler = CL.sourceList [1..1000000]
                     $$ CL.map (\c -> ("aaaa", c))
                     =$ CL.map (\(t, c) -> t <> " " <> BB.intDec c <> "\n")
                     =$ CL.map BB.toLazyByteString
                     =$ CL.map BL.toStrict
                     =$ CB.sinkFile "IOPartialSorting.txt" 
      runResourceT filler
