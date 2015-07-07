{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, InstanceSigs, FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MonadTry.ZipList where


import Control.Applicative


test :: IO ()
test = do

   -- ^ Example using ZipList monad
   print $ (\a b c -> a * b + c)
      <$> ZipList [1 .. 4 :: Int]
      <*> ZipList (cycle [-1, 1])
      <*> ZipList [4, 3 .. 1]

   -- ^ Example using ParallelListComp
   print [(x * y, z)
            | x <- [1..3 :: Int]
            | y <- [4, 3 .. 1]
            | z <- ['a'..'z']]
   
   -- ^ Example using variadic arguments
   print (variadicList 1 2 3 :: [Int])
   print $ (\a b c -> sum $ variadicList a b c)
      <$> ZipList [1 .. 4 :: Int]
      <*> ZipList (cycle [-1, 1])
      <*> ZipList [4, 3 .. 1]


class BuildList a r | r -> a where
   buildList :: [a] -> r

instance BuildList a [a] where
   buildList = reverse

instance BuildList a r => BuildList a (a -> r) where
   buildList :: [a] -> a -> r
   buildList l x = buildList (x:l)

variadicList :: (BuildList a r) => r
variadicList = buildList []


{-
class BuildList2 r a where
   buildList2 :: r -> [a]

instance BuildList2 a a where
   buildList2 x = [x]

instance BuildList2 r a => BuildList2 (a -> r) a where
   buildList2 :: (a -> r) -> [a]
   buildList2 = undefined -- Hard to define...
-}

   
-- TODO : play with pattern like template method and factories (in other file)
