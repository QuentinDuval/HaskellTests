{-# LANGUAGE MultiWayIf, FlexibleContexts, OverloadedStrings #-}
module MonadTry.ContMonad where


import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Writer
import Data.Text as T


-- | A monoid instance for Maybe that acts as an alternative 

newtype AltMaybe a = AltMaybe { alt :: Maybe a }
   deriving (Show, Eq, Ord)

instance (Monoid a) => Monoid (AltMaybe a) where
   mempty = AltMaybe . Just $ mempty
   mappend (AltMaybe (Just a)) (AltMaybe (Just b)) = AltMaybe $ Just $ a <> b
   mappend _ _ = AltMaybe Nothing


-- | Example of a parser based on continuation
-- | This example can be generalized for any kind of behavior needing callbacks 

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
      handle OpenBlock   = withIndent (0, 1)  "{\n"
      handle CloseBlock  = withIndent (-1, 0) "}\n"
      handle (Content c) = withIndent (0, 0)  (singleton c <> ";\n")
      handle Stop = do
         v <- get
         return $ if v == 0
            then Just ""
            else Nothing
      
      withIndent (b, a) c = do
         modify (+b)
         depth <- get
         modify (+a)
         return . Just $ T.replicate depth "  " <> c


sinkHandler :: (MonadWriter (AltMaybe Text) m) => Maybe Text -> m ()
sinkHandler = tell . AltMaybe


-- | Testing this parser (with a all-or-nothing success)

testCase :: Text -> IO (Maybe Text)
testCase t = do
   r <- runWriterT $
      flip runStateT 0 $
         runContT (parseEngine t >>= indentHandler) sinkHandler
   return $ alt $ snd r

test :: IO ()
test = do
   print =<< testCase "[a[ab]c]"
   print =<< testCase "[a[ab]c]]"
   print =<< testCase "[[a[ab]c]"

