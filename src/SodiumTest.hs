{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiWayIf #-}
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



data Sources = Sources {
   setText :: Text -> Reactive (),
   setBool :: Bool -> Reactive ()
}

data Sinks = Sinks {
   eventS1 :: Event Text,
   eventS2 :: Event Text,
   eventS3 :: Event Text
}


-- TODO - Run some kind of example with user inputs + a mute on someone + filter one some other variable

exampleWorkflow :: Reactive (Sources, Sinks)
exampleWorkflow = do
   (eventSink, eventSource)   <- newEvent
   (behavior, behaviorSource) <- newBehavior True
   -- snapshot to associate an event with a behavior
   -- hold to transform an event into a behavior
   return (Sources
            { setText = eventSource
            , setBool = behaviorSource},
           Sinks
            { eventS1 = eventSink
            , eventS2 = gate eventSink behavior
            , eventS3 = filterE (\t -> T.length t <= 3) eventSink})


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
      

userSink :: (MonadIO m) => Sources -> Sink Text m ()
userSink input = awaitForever $ \t ->
   liftIO $ sync $
      case t of
         "on"   -> setBool input True
         "off"  -> setBool input False
         _      -> setText input t


test :: IO()
test = do
   let listener s = B.putStrLn . encodeUtf8 . (<>) (s <> ": ")
   input <- sync $ do
      (sources, sinks) <- exampleWorkflow
      void $ listen (eventS1 sinks) (listener "l1")
      void $ listen (eventS2 sinks) (listener "l2")
      void $ listen (eventS3 sinks) (listener "l3")
      return sources
   runConduit $ userInput $$ stopIf ("exit" ==) $= userSink input
