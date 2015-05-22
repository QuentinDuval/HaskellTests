{-# LANGUAGE OverloadedStrings #-}
module MarkovChain where

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CList
import Data.MarkovChain
import Data.Monoid
import Data.Text(Text)
import Data.Text.Lazy.Builder as B
import qualified Data.Text as T
import Data.Text.Encoding(encodeUtf8)
import Data.Text.Lazy(toStrict)
import System.IO
import System.Random


-- | Markov chain used to generate an output (modeled as a pipe for efficiency)

data Token = Word Text | LineFeed deriving (Show, Eq, Ord)

tokenize :: Text -> [Token]
tokenize t = concatMap lineToTokens (splitLines t)
   where
      splitLines = T.splitOn "\n"
      lineToTokens = (++ [LineFeed]) . map Word . T.words

infiniteSource :: (RandomGen g, Monad m) => Int -> [String] -> g -> Source m Token
infiniteSource ctxSize sources =
   let concatSources = toStrict . B.toLazyText . mconcat . fmap B.fromString $ sources
   in  CList.sourceList . run ctxSize (tokenize concatSources) 0

tokenToString :: (Monad m) => Conduit Token m B.Builder
tokenToString = awaitForever $ \t -> yield $ case t of
   Word w   -> B.fromText w <> " "
   LineFeed -> "\n"

markovGeneration :: Int -> [FilePath] -> FilePath -> Int -> IO ()
markovGeneration ctxSize inputPath outputPath outSize = do
   g <- getStdGen
   sources <- mapM readFile inputPath
   withFile outputPath WriteMode $ \output -> 
      runConduit $
         infiniteSource ctxSize sources g $$ CList.isolate outSize $= tokenToString
         $= CList.map (encodeUtf8 . toStrict . B.toLazyText) $= sinkHandle output


test :: IO ()
test = markovGeneration 3 ["inputMarkov.txt"] "outputMarkov.txt" 5000
   

