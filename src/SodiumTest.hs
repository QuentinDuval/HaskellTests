{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module SodiumTest where

import            Control.Monad
import            Data.Text(Text)
import qualified  Data.Text as T
import            FRP.Sodium




data Ret = Ret {
   actions :: [IO()],
   setText :: Text -> Reactive (),
   setBool :: Bool -> Reactive ()
}

example :: (Text -> IO()) -> (Text -> IO()) -> Reactive Ret
example l1 l2 = do
   (eventSink :: Event Text, eventSource) <- newEvent
   (behavior, behaviorSource) <- newBehavior True
   let filteredEventSink = gate eventSink behavior
   l1' <- listen eventSink l1
   l2' <- listen filteredEventSink l2
   -- snapshot to associate an event with a behavior
   -- hold to transform an event into a behavior
   return Ret {
      actions = [l1', l2'],
      setText = eventSource,
      setBool = behaviorSource }



test :: IO()
test = do
   let l1 = \(t :: Text) -> putStrLn ("l1: " ++ T.unpack t)
   let l2 = \(t :: Text) -> putStrLn ("l2: " ++ T.unpack t)
   ret <- sync $ example l1 l2
   sync $ setText ret "a"
   sync $ setBool ret False
   sync $ setText ret "b"
   void $ sequence (actions ret)
   putStrLn "End"

