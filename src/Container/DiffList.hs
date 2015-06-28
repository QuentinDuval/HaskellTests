module Container.DiffList where



import qualified Data.DList as DL


test :: IO ()
test =
   do dl <- loop DL.empty
      print $ DL.toList dl
   where
      loop s = do
         i <- getLine
         if i == "quit"
            then return s
            else loop (DL.append s $ DL.fromList i)
   
   




