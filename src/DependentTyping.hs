{-# LANGUAGE TypeFamilies, EmptyDataDecls, DataKinds, UndecidableInstances,
    GADTs, RankNTypes, FlexibleInstances, FlexibleContexts #-}

module DependentTyping where



data Nat = Zero | Succ Nat

type One    = Succ Zero
type Two    = Succ One
type Three  = Succ Two


type family Plus a b where
   Plus x Zero     = x
   Plus x (Succ y) = Succ (Plus x y) 


data Vector a (s :: Nat) where
   NilV :: Vector a Zero
   Cons :: a -> Vector a n -> Vector a (Succ n)
   Snoc :: Vector a n -> a -> Vector a (Succ n)


data Matrix a (m :: Nat) (n :: Nat) where
   Row      :: Vector a (Succ n) -> Matrix a One (Succ n)
   AddRow   :: Matrix a m n -> Matrix a One n -> Matrix a (Succ m) n


testMatrix :: Matrix Int Three Two
testMatrix =
   let r1 = Row (Cons 1 (Cons (2::Int) NilV))
       r2 = Row (Snoc (Snoc NilV 3) (4::Int))
       r3 = Row (Cons 5 (Cons (6::Int) NilV))
   in AddRow (AddRow r1 r2) r3


type family ItemT v
type instance ItemT (Vector a n) = a
type instance ItemT (Matrix a m n) = a


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


instance (Show a, ToList (Vector a n)) => Show (Vector a n) where
   show v = show (toList v)

 


