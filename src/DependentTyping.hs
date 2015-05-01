{-# LANGUAGE TypeFamilies, EmptyDataDecls, DataKinds, GADTs, RankNTypes #-}

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
   Mult     :: Matrix a m k -> Matrix a k n -> Matrix a m n


testMatrix :: Matrix Int Three Two
testMatrix =
   let r1 = Row (Cons 1 (Cons (2::Int) NilV))
       r2 = Row (Snoc (Snoc NilV 3) (4::Int))
       r3 = Row (Cons 5 (Cons (6::Int) NilV))
   in AddRow (AddRow r1 r2) r3


-- toList :: Vector  

