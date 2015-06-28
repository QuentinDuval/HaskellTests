module MonadTry.ListMonad where

import Control.Arrow
import Control.Applicative
import Control.Monad
import qualified Data.Set as S


type QueenResult = [Int]

eightQueens :: Int -> [QueenResult]
eightQueens n = fst <$> foldM addQueen ([], S.fromList [1..n]) [1..n]
   where
      addQueen ~(solution, remaining) _ = do
         v <- S.toList remaining
         let notOnDiag (x, y) = x /= abs (v - y) 
         guard $ all notOnDiag $ zip [1..] solution
         return (v : solution, S.delete v remaining)



eightQueens' :: Int -> [QueenResult]
eightQueens' n = fst <$> foldM addQueen ([], [1..n]) [1..n]
   where
      addQueen ~(solution, remaining) _ = do
         (v, vs) <- selectOne remaining
         let notOnDiag (x, y) = x /= abs (v - y) 
         guard $ all notOnDiag $ zip [1..] solution
         return (v : solution, vs)


-- | Select one element and return the rest, in the list monad
selectOne :: [a] -> [(a, [a])]
selectOne []     = []
selectOne (x:xs) = (x,xs) : map (second (x:)) (selectOne xs)
         
   


