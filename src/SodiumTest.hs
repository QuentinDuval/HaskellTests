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
   eventS2 :: Event Text 
}


-- TODO - Run some kind of example with user inputs + a mute on someone + filter one some other variable

exampleWorkflow :: Reactive (Sources, Sinks)
exampleWorkflow = do
   (eventSink, eventSource)   <- newEvent
   (behavior, behaviorSource) <- newBehavior True
   -- snapshot to associate an event with a behavior
   -- hold to transform an event into a behavior
   return (Sources { setText = eventSource, setBool = behaviorSource},
           Sinks { eventS1 = eventSink, eventS2 = gate eventSink behavior })


test :: IO()
test = do
   let l1 = \(t :: Text) -> putStrLn ("l1: " ++ T.unpack t)
   let l2 = \(t :: Text) -> putStrLn ("l2: " ++ T.unpack t)
   
   input <- sync $ do
      (sources, sinks) <- exampleWorkflow
      void $ listen (eventS1 sinks) l1
      void $ listen (eventS2 sinks) l2
      return sources
      
   sync $ setText input "a"
   sync $ setBool input False
   sync $ setText input "b"
   putStrLn "End"

