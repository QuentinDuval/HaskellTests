{-# LANGUAGE MultiWayIf #-}
module Container.Zipper where


import Data.List.Zipper



test :: IO ()
test =
   do let l = ['a' .. 'z']
      loop (fromList l)
   where
      loop z = do
         putStrLn "Left (l) / Right (r) / Delete (d):"
         i <- getLine
         let z' = if | i == "r" -> right z
                     | i == "l" -> left z
                     | i == "d" -> delete z
                     | otherwise -> z
         print $ cursor z'
         loop z'
