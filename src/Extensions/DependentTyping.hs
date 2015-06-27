{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances,
    GADTs, RankNTypes, FlexibleInstances, FlexibleContexts #-}

module Extensions.DependentTyping where

import Control.Monad.IO.Class



-- | Definition of natural numbers

data Nat = Zero | Succ Nat

type One    = Succ Zero
type Two    = Succ One
type Three  = Succ Two


-- | Close type families to implement function at type level

type family Plus a b where
   Plus x Zero     = x
   Plus x (Succ y) = Succ (Plus x y) 


-- | Using DataKinds and GADT to build dependent typed vector and matrix 

data Vector a (s :: Nat) where
   NilV :: Vector a Zero
   Cons :: a -> Vector a n -> Vector a (Succ n)
   Snoc :: Vector a n -> a -> Vector a (Succ n)


data Matrix a (m :: Nat) (n :: Nat) where
   Row      :: Vector a (Succ n) -> Matrix a One (Succ n)
   AddRow   :: Matrix a m n -> Matrix a One n -> Matrix a (Succ m) n



-- | Open type families to retrieve the item types

type family ItemT v
type instance ItemT (Vector a n) = a
type instance ItemT (Matrix a m n) = a


-- | ToList: using some kind of logic programming paradigm

class ToList v where
   toList :: v -> [ItemT v]

instance ToList (Vector a Zero) where
   toList _ = []

instance (ToList (Vector a n)) => ToList (Vector a (Succ n)) where
   toList (Cons a v) = a : toList v
   toList (Snoc v a) = toList v ++ [a]

instance ToList (Matrix a Zero m) where
   toList _ = []

instance (ToList (Matrix a m n), ToList (Vector a n)) => ToList (Matrix a (Succ m) n) where
   toList (Row v)      = toList v
   toList (AddRow m r) = toList m ++ toList r


-- | Show instances

instance (Show a, ToList (Vector a n)) => Show (Vector a n) where
   show v = show (toList v)

instance (Show a, ToList (Matrix a m n)) => Show (Matrix a m n) where
   show v = show (toList v)


-- | Test code (if it compiles, it nearly surely works)

testVector :: Vector Int Three
testVector = Cons 1 (Snoc (Cons 2 NilV) 3)

testMatrix :: Matrix Int Three Two
testMatrix =
   let r1 = Row (Cons 1 (Cons (2::Int) NilV))
       r2 = Row (Snoc (Snoc NilV 3) (4::Int))
       r3 = Row (Cons 5 (Cons (6::Int) NilV))
   in AddRow (AddRow r1 r2) r3

testDT :: IO ()
testDT = do 
   print testVector
   print testMatrix

