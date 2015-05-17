{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiWayIf, LambdaCase #-}
module SodiumTest where

import            Control.Applicative
import            Control.Monad
import            Control.Monad.IO.Class
import qualified  Data.ByteString.Char8 as B
import            Data.Conduit as C
import            Data.Monoid((<>))
import            Data.Text(Text)
import qualified  Data.Text as T
import            Data.Text.Encoding(decodeUtf8, encodeUtf8)
import            FRP.Sodium



-- TODO - Behavior might contain some function, not only values... you could use a behavior map function on an event
-- TODO - Run some kind of example with user inputs + a mute on someone + filter one some other variable
-- TODO - Function that adds or concat elements
-- TODO - Snapshot might be used to apply a behavior function on an event!
-- TODO - sync creates some kind of transaction: changes are performed after its complete -> how to send new event? create an oscilator


{-| Generic part, used for tests -}

type NetworkInput a = a -> Reactive ()

userInput :: (MonadIO m) => Source m Text
userInput = forever $
   yield =<< decodeUtf8 <$> B.init <$> liftIO B.getLine

stopIf :: (Monad m) => (a -> Bool) -> Conduit a m a
stopIf stopCond = loop where
   loop = do
      v <- await
      case v of
         Just t  -> unless (stopCond t) $ yield t >> loop
         Nothing -> return ()

networkCon :: (MonadIO m) => NetworkInput a -> Sink a m ()
networkCon networkInput = awaitForever (liftIO . sync . networkInput)

listener :: Text -> Text -> IO()
listener s = B.putStrLn . encodeUtf8 . (s <>)

partitionE :: (a -> Bool) -> Event a -> (Event a, Event a)
partitionE p e = (filterE p e, filterE (not . p) e)


{-| Reactive network 0 -}

data Network1 = Network1 {
   textIn    :: NetworkInput Text,
   statusIn  :: NetworkInput Bool,
   caseModIn :: NetworkInput Bool,
   textOut   :: Event Text,
   statusOut :: Event Text }


network1 :: Reactive Network1
network1 = do
   (textSink, textSource) <- newEvent
   (gateStatus, statusSource) <- newBehavior True
   (caseStatus, caseModSource) <- newBehavior False
   let condUpper t b = if b then T.toUpper t else t
   let textOutput = snapshot condUpper (gate textSink gateStatus) caseStatus
   return Network1 {
      textIn = textSource,
      statusIn = statusSource,
      caseModIn = caseModSource,
      textOut = textOutput,
      statusOut = (T.pack . show) <$> value gateStatus }


networkCon1 :: (MonadIO m) => Network1 -> Sink Text m ()
networkCon1 network = do
   liftIO . sync $ do
      void $ listen (textOut network) (listener "> ")
      void $ listen (statusOut network) (listener "On/off: ")
   awaitForever $ \t ->
      liftIO . sync $ case t of
         "on"   -> statusIn network True
         "off"  -> statusIn network False
         "up"   -> caseModIn network True
         "down" -> caseModIn network False
         _      -> textIn network t


test1 :: IO ()
test1 = do
   network <- sync network1
   runConduit $ userInput $$ stopIf ("exit" ==) $= networkCon1 network


{-| Reactive network 2 -}

test2 :: IO()
test2 = do
   undefined
