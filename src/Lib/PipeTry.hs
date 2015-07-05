module Lib.PipeTry where

import Data.Set as S
import Pipes
import qualified Pipes.Prelude as Pipes


test :: IO ()
test = do

   -- ^ Works for any Foldable
   runEffect $ for (each              [1 .. 4 :: Int]) (lift . putStr . show)
   runEffect $ for (each $ S.fromList [1 .. 4 :: Int]) (lift . putStr . show)
   
   -- Understanding for:
   -- for producer yield = producer
   -- for (yield x) f    = f x
   
   -- ^ Using: (f ~> g) x = for (f x) g
   runEffect $ (each ~> lift . putStr . show) [1 .. 4 :: Int]
   
   
   -- ^ Equivalent formulations
   runEffect $ lift getLine  >~  Pipes.takeWhile (/= "exit") >-> Pipes.stdoutLn
   runEffect $ Pipes.stdinLn >-> Pipes.takeWhile (/= "exit") >-> Pipes.stdoutLn
   
   -- (f ~> g) x = for (f x) g
   
   putStrLn "End"



