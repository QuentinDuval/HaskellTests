{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

module MultiwayIf where

import Control.Applicative
import Data.Text



-- | Infinite list of fizz buzz

fizzBuzz :: [Text]
fizzBuzz = fizzBuzz' <$> [1 :: Int ..] where
   fizzBuzz' n =
      let [fCond, bCond] = (== 0) . (n `rem`) <$> [3, 5]
      in if | fCond && bCond -> "FizzBuzz"
            | fCond -> "Fizz"
            | bCond -> "Buzz"
            | otherwise -> pack $ show n
    



