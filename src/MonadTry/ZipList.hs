{-# LANGUAGE ParallelListComp #-}
module MonadTry.ZipList where


import Control.Applicative


test :: IO ()
test = do
   print $ (\a b c -> a * b + c)
      <$> ZipList [1 .. 4 :: Int]
      <*> ZipList (cycle [-1, 1])
      <*> ZipList [4, 3 .. 1]

   print [(x,y) | x <- [1..3 :: Int]
                | y <- ['a'..'c']]
   
   -- TODO : use variadic transformation to a list?
