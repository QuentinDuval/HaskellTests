{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module SodiumTest where

import            Control.Monad
import            Data.Text(Text)
import qualified  Data.Text as T
import            FRP.Sodium




data Ret = Ret {
   setText :: Text -> Reactive (),
   setBool :: Bool -> Reactive (),
   eventS1 :: Event Text,
   eventS2 :: Event Text 
}


-- TODO - Run some kind of example with user inputs + a mute on someone + filter one some other variable

exampleWorkflow :: Reactive Ret
exampleWorkflow = do
   (eventSink, eventSource) <- newEvent
   (behavior, behaviorSource) <- newBehavior True
   -- snapshot to associate an event with a behavior
   -- hold to transform an event into a behavior
   return Ret {
      setText = eventSource,
      setBool = behaviorSource,
      eventS1 = eventSink,
      eventS2 = gate eventSink behavior }



test :: IO()
test = do
   let l1 = \(t :: Text) -> putStrLn ("l1: " ++ T.unpack t)
   let l2 = \(t :: Text) -> putStrLn ("l2: " ++ T.unpack t)
   
   ret <- sync $ do
      w <- exampleWorkflow
      void $ listen (eventS1 w) l1
      void $ listen (eventS2 w) l2
      return w
      
   sync $ setText ret "a"
   sync $ setBool ret False
   sync $ setText ret "b"
   putStrLn "End"

