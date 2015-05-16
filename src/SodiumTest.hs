{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module SodiumTest where


import            Control.Monad
import            Data.Text(Text)
import qualified  Data.Text as T
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


test :: IO()
test = do
   let listener s = putStrLn . ((s ++ ": ") ++) . T.unpack
   
   input <- sync $ do
      (sources, sinks) <- exampleWorkflow
      void $ listen (eventS1 sinks) (listener "l1")
      void $ listen (eventS2 sinks) (listener "l2")
      void $ listen (eventS3 sinks) (listener "l3")
      return sources
      
   mapM_ (sync . setText input) ["Trying", "out"]
   sync  (setBool input False)
   mapM_ (sync . setText input) ["Sodium", "!"]
