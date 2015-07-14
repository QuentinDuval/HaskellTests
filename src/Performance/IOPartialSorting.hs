{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Performance.IOPartialSorting where

import Control.Applicative
import Control.Arrow

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State(modify')

import qualified Data.Attoparsec.Text as P

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB

import Data.Char

import Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Lift

import qualified Data.PQueue.Prio.Min as H

import Data.List as L
import Data.List.Split(chunksOf)
import Data.Monoid
import Data.Ord

import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding

import Data.Tuple(swap)
import qualified Data.Word8 as W8

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
   inputs <- chunksOf 2 <$> B.splitWith W8.isSpace <$> B.readFile "IOPartialSorting.txt"
   let decoded = fmap (fmap decodeUtf8) inputs
   let dataSet = (\(x:y:_) -> (x, read (T.unpack y) :: Int)) <$> decoded
   let res = take 5000 $ sortBy (flip $ comparing snd) dataSet
   print res


-- | Test with parsec

testParsec :: IO ()
testParsec =
   do inputs <- decodeUtf8 <$> B.readFile "IOPartialSorting.txt"
      let (Right dataSet) = P.parseOnly (many readLine) inputs
      let res = take 5000 $ sortBy (flip $ comparing snd) dataSet
      print res
   
readLine :: P.Parser (Text, Int)
readLine = do
   txt <- P.takeWhile (not . isSpace) <* P.space
   val <- P.decimal <* P.skipWhile isSpace
   return (txt, val)


-- | Trying to get faster with conduit

testConduit :: IO ()
testConduit = withFile "IOPartialSorting.txt" ReadMode $ \h -> do
      res <- runConduit $
               CB.sourceHandle h
               $$ CB.lines
               =$ CL.map decodeUtf8
               =$ CL.map (P.parseOnly readLine)
               =$ CL.map (\(Right r) -> r)
               =$ computeRes 5000
      print res
      
      where
         addToHeap :: Int -> (Text, Int) -> H.MinPQueue Int Text -> H.MinPQueue Int Text
         addToHeap nb pair heap =
            let (v, p) = pair
                (h, _) = H.findMin heap
            in if H.size heap < nb
               then H.insert p v heap
               else if h > p
                  then heap
                  else H.insert p v (H.deleteMin heap)
      
         computeRes :: (Monad m) => Int -> Sink (Text, Int) m [(Text, Int)]
         computeRes nb = do
            r <- execStateLC H.empty $ awaitForever (lift . modify' . addToHeap nb)
            return $ reverse $ swap <$> H.take nb r


-- | To fill an input file

fillInput :: IO ()
fillInput =
   do let filler = CL.sourceList ([1..500000] ++ (reverse [500000..1000000]))
                     $$ CL.map (\c -> ("aaaa", c))
                     =$ CL.map (\(t, c) -> t <> " " <> BB.intDec c <> "\n")
                     =$ CL.map BB.toLazyByteString
                     =$ CL.map BL.toStrict
                     =$ CB.sinkFile "IOPartialSorting.txt" 
      runResourceT filler
