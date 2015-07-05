module Lib.DateTime where


import Data.Time


test :: IO ()
test = do
   dt <- getCurrentTime
   let (y,m,d) = toGregorian $ utctDay dt
   print dt
   print (y, m, d)
