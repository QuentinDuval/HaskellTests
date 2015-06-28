{-# LANGUAGE MultiWayIf, FlexibleContexts, OverloadedStrings #-}
module MonadTry.ContMonad where


import Control.Monad.Cont
import Control.Monad.State
import Data.Monoid
import Data.Text



data ParseEvent = OpenBlock | CloseBlock | Stop | Content Char

parseEngine :: (Monad m) => Text -> ContT r m ParseEvent
parseEngine txt = ContT $ \handler ->
   case uncons txt of
      Nothing     -> handler Stop
      Just (h, t) -> do
         if | h == '['  -> handler OpenBlock
            | h == ']'  -> handler CloseBlock
            | otherwise -> handler (Content h)
         runContT (parseEngine t) handler



parseHandler :: (MonadState Text m) => ParseEvent -> ContT r m Text
parseHandler OpenBlock   = modify (<> "((") >> get
parseHandler CloseBlock  = modify (<> "))") >> get
parseHandler (Content c) = modify (<> singleton c) >> get
parseHandler Stop        = get


sinkHandler :: (MonadIO m, Show a) => a -> m ()
sinkHandler = liftIO . print


test :: IO ()
test = do
   flip runStateT "" $
      runContT (parseEngine "[a[ab]c]" >>= parseHandler) sinkHandler
   return ()

