{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf, PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module TextAlgo where

import            Control.Monad.Error
import            Data.Char
import            Data.Text(Text)
import qualified  Data.Text as T


data ExprError = UnknownToken | MissingOperator
   deriving (Show)


dijkstraTwoStack :: (MonadError ExprError m) => Text -> m Int
dijkstraTwoStack text = loop text [] where
   
   readVal txt vals =
      let (n,t) = T.span isDigit txt
      in loop t (read (T.unpack n):vals)
      
   loop txt vals
      | Just(h, t) <- T.uncons txt =
         let (x:y:vs) = vals
         in if
            | h `elem` "( )" -> loop t vals
            | h == '+'  -> loop t (x + y  :vs)
            | h == '-'  -> loop t (x - y  :vs)
            | h == '*'  -> loop t (x * y  :vs)
            | h == '/'  -> loop t (div x y:vs)
            | isDigit h -> readVal txt vals
            | otherwise -> throwError UnknownToken
      | (_:_:_) <- vals = throwError MissingOperator
      | otherwise       = return $ head vals
   

test :: String -> Either ExprError Int
test s = dijkstraTwoStack $ T.pack s

test' :: Either ExprError Int
test' = test "(2 1 +) 3 * 21 +"


