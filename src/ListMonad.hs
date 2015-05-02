module ListMonad where

import Control.Applicative
import Control.Monad
import qualified Data.Set as S


type QueenResult = [Int]

eightQueens :: Int -> [QueenResult]
eightQueens n = fst <$> foldM addQueen ([], S.fromList [1..n]) [1..n]
   where
      addQueen ~(solution, left) _ = do
         v <- S.toList left
         let notOnDiag (x, y) = x /= abs (v - y) 
         guard $ all notOnDiag $ zip [1, 2..] solution
         return (v : solution, S.delete v left)

