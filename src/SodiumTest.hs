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

data Network0 = Network0 {
   textIn    :: NetworkInput Text,
   textOut   :: Event Text,
   statusOut :: Event Text }

network0 :: Reactive Network0
network0 = do
   (nSink, nSource) <- newEvent
   
   let statusEvent = (\case "on" -> Just True;
                            "off" -> Just False;
                            _ -> Nothing) <$> nSink

   let caseEvent = (\case "up"   -> Just True;
                          "down" -> Just False;
                          _      -> Nothing) <$> nSink

   let eSink = {-filterE (`notElem` ["on", "off", "up", "down"])-} nSink
   
   gateStatus <- hold True (filterJust statusEvent)
   caseStatus <- hold False (filterJust caseEvent)
   
   let condUpper t b = if b then T.toUpper t else t
   let textOutput = snapshot condUpper (gate eSink gateStatus) caseStatus
   
   return Network0 {
      textIn = nSource,
      textOut = textOutput,
      statusOut = (T.pack . show) <$> value gateStatus }


test0 :: IO ()
test0 = do
   input <- sync $ do
      network <- network0
      void $ listen (textOut network)   (listener "> ")
      void $ listen (statusOut network) (listener "On/off: ")
      return (textIn network)
   runConduit $ userInput $$ stopIf ("exit" ==) $= networkCon input


{-| Reactive network 1 -}

data Sources = Sources {
   setText :: Text -> Reactive (),
   setBool :: Bool -> Reactive ()
}

data Sinks = Sinks {
   eventS1 :: Event Text,
   eventS2 :: Event Text,
   eventS3 :: Event Text,
   eventS4 :: Event Text
}

reactiveNetwork :: Reactive (Sources, Sinks)
reactiveNetwork = do
   (eventSink, eventSource)   <- newEvent
   (behavior, behaviorSource) <- newBehavior True
   let gateEventSink = (T.pack . show) <$> value behavior
   -- snapshot to associate an event with a behavior
   -- hold to transform an event into a behavior
   return (Sources
            { setText = eventSource
            , setBool = behaviorSource},
           Sinks
            { eventS1 = eventSink
            , eventS2 = gate eventSink behavior
            , eventS3 = filterE (\t -> T.length t <= 3) eventSink
            , eventS4 = gateEventSink})

userSink :: (MonadIO m) => Sources -> Sink Text m ()
userSink input = awaitForever $ \t ->
   liftIO $ sync $
      case t of
         "on"  -> setBool input True
         "off" -> setBool input False
         _     -> setText input t

test1 :: IO()
test1 = do
   input <- sync $ do
      (sources, sinks) <- reactiveNetwork
      void $ listen (eventS4 sinks) (listener "status")
      void $ listen (eventS1 sinks) (listener "l1")
      void $ listen (eventS2 sinks) (listener "l2")
      void $ listen (eventS3 sinks) (listener "l3")
      return sources
   runConduit $ userInput $$ stopIf ("exit" ==) $= userSink input


{-| Reactive network 2 -}

test2 :: IO()
test2 = do
   undefined
