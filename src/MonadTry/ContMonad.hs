{-# LANGUAGE MultiWayIf, FlexibleContexts, OverloadedStrings #-}
module MonadTry.ContMonad where


import Control.Monad.Cont
import Control.Monad.State
import Data.Monoid
import Data.Text as T



data ParseEvent = OpenBlock | CloseBlock | Stop | Content Char

parseEngine :: (Monad m) => Text -> ContT r m ParseEvent
parseEngine txt =
   ContT $ \handler ->
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


indentHandler :: (MonadState Int m) => ParseEvent -> ContT r m Text
indentHandler = handle
   where
      handle OpenBlock   = do
         r <- withIndent "{"
         modIndent 1
         return r
      
      handle CloseBlock  = modIndent (-1) >> withIndent "}"
      handle (Content c) = withIndent $ singleton c <> ";"
      handle Stop = do
         v <- get
         return $ if v == 0
            then "[Success]"
            else "[Failure]"
      
      modIndent i = modify (+ i)
      withIndent c = do
         i <- get
         return $ (T.replicate i "  ") <> c


sinkHandler :: (MonadIO m, Show a) => a -> m ()
sinkHandler = liftIO . print


test :: IO ()
test = do
   flip runStateT 0 $
      runContT (parseEngine "[a[ab]c]" >>= indentHandler) sinkHandler
   return ()

