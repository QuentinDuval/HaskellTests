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


indentHandler :: (MonadState Int m) => ParseEvent -> ContT r m (Maybe Text)
indentHandler = handle
   where
      handle OpenBlock   = withIndent (0, 1)  "{"
      handle CloseBlock  = withIndent (-1, 0) "}"
      handle (Content c) = withIndent (0, 0)  (singleton c <> ";")
      handle Stop = do
         v <- get
         return $ if v == 0 then Just "" else Nothing
      
      withIndent (b, a) c = do
         modify (+b)
         depth <- get
         modify (+a)
         return . Just $ T.replicate depth "  " <> c


sinkHandler :: (MonadIO m, Show a) => a -> m ()
sinkHandler = liftIO . print


test :: IO ()
test = do
   flip runStateT (0 :: Int) $
      runContT (parseEngine "[a[ab]c]" >>= indentHandler) (sinkHandler)
   return ()

