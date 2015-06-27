module Container.DataSequence where

import Data.Monoid
import Data.Sequence as S




test :: IO ()
test = do
   let s = S.fromList [1..1000]
   let e = S.fromList [1..1000]
   print (s <> e)
   return ()


