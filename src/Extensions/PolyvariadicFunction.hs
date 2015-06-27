{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, OverloadedStrings, InstanceSigs #-}

module Extensions.PolyvariadicFunction where

import Data.Text


-- Improving the solution from:
-- From http://stackoverflow.com/questions/3467279/how-to-create-a-polyvariadic-haskell-function

class SumRes a r where 
    sumOf :: a  -> r


-- For integers

instance SumRes Integer Integer where
   sumOf :: Integer -> Integer
   sumOf = id

instance (Integral a, SumRes Integer r) => SumRes Integer (a -> r) where
   sumOf :: Integer -> a -> r
   sumOf x = sumOf . (x +) . toInteger


-- For strings

instance SumRes Text Text where
    sumOf = id

instance (SumRes Text r) => SumRes Text (Text -> r) where
    sumOf x = sumOf . append  x


test1 :: Integer
test1 = sumOf (1 :: Integer) 2 3 4 5

test2 :: Text
test2 = sumOf ("toto" :: Text) ("titi" :: Text) ("tutu" :: Text)

