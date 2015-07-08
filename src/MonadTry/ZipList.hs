{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE InstanceSigs, FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MonadTry.ZipList where


import Control.Applicative
import Data.List(transpose)


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
   
   
   -- ^ Example using transpose (does not support cycle though)
   let inputs = [[1..3 :: Int], [4, 3..2], [-1, 1, -1]]
   print $ sum <$> transpose inputs
   
   
   -- ^ Example using variadic arguments
   -- ^ In the end pretty useless
   
   -- sum . variadicList does not work... because you somehow need rank2types?
   let sum' a b c = sum $ variadicList a b c
   print $ sum'
      <$> ZipList [1 .. 4 :: Int]
      <*> ZipList (cycle [-1, 1])
      <*> ZipList [4, 3 .. 1]
   
   putStrLn $ vList 'x' 'y' 'z' -- Signature not needed
   
   let sum'' a b c = (vFoldl (+) 0 a b c :: Int) -- Signature is needed for compilation
   print [sum'' x y z
            | x <- [1..3 :: Int]
            | y <- [4, 3 .. 1 :: Int]
            | z <- [1..4 :: Int]]

   -- Worse: this does not work.
   {-
   print $ sum'
      <$> ZipList [1 .. 4 :: Int]
      <*> ZipList (cycle [-1, 1])
      <*> ZipList [4, 3 .. 1]
   -}


-- | Variadic list builder

class BuildList a r | r -> a where
   buildList :: [a] -> r

instance BuildList a [a] where
   buildList = reverse

instance BuildList a r => BuildList a (a -> r) where
   buildList :: [a] -> a -> r
   buildList l x = buildList (x:l)

variadicList :: (BuildList a r) => r
variadicList = buildList []


-- | Trying the variadic from:
-- | http://w3facility.org/question/polyvariadic-functions-in-haskell/

class Variadic a b r | r -> a where
   variadic :: ([a] -> b) -> r

instance Variadic a b (a -> b) where
   variadic :: ([a] -> b) -> a -> b
   variadic f x = f [x]

instance Variadic a b (a -> r) => Variadic a b (a -> a -> r) where
   variadic :: ([a] -> b) -> a -> a -> r
   variadic f x y = variadic (f . (x:)) y

vList :: (Variadic a [a] r) => r
vList = variadic id

vFoldl :: (Variadic b a r) => (a -> b -> a) -> a -> r
vFoldl f z = variadic (foldl f z)

vConcat :: (Variadic [a] [a] r) => r
vConcat = vFoldl (++) []



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
