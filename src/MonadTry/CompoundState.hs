{-# LANGUAGE MultiWayIf, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module MonadTry.CompoundState where

import Control.Lens
import Control.Monad
--import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Conduit
import Data.Conduit.List
import Data.Text as T


{- |
Given a getter (a -> b), and a setter (a -> b -> a)
transform the current state from MonadState a to MonadState b.
-}
withSubState :: Monad m => (s1 -> s2) -> (s1 -> s2 -> s1) -> StateT s2 m a -> StateT s1 m a
withSubState getter setter f = do
   fullState <- get
   let projState = getter fullState
   (res, newState) <- lift $ runStateT f projState
   put (setter fullState newState)
   return res


-- | CHECK - It might be the same as Control.Lens.Zoom
-- | zoom :: Monad m             => Lens' s t      -> StateT t m a -> StateT s m a


-- | Test
-- | Based on the fact that:
-- MonadState s m => MonadState s (ConduitM i o m)
-- MonadTrans (ConduitM i o)


countShort :: (Monad m) => Conduit Text (StateT Int m) Text
countShort =
   awaitForever $ \t -> do
      when (T.length t > 5) $
         lift $ modify (+1)
      yield t


countShort' :: (Monad m) => Conduit Text (StateT ([Text], Int) m) Text
countShort' =
   awaitForever $ \t -> do
      when (T.length t > 5) $
         lift $ zoom _2 $ modify (+1)
      yield t


keepPalindroms :: (Monad m) => Conduit Text (StateT [Text] m) Text
keepPalindroms =
   awaitForever $ \t -> do
      when (T.reverse t == t) $
         lift $ modify (t:)
      yield t


keepPalindroms' :: (Monad m) => Conduit Text (StateT ([Text], Int) m) Text
keepPalindroms' =
   awaitForever $ \t -> do
      when (T.reverse t == t) $
         lift $ zoom _1 $ modify (t:)
      yield t


test :: IO ()
test = do
   print =<< (flip execStateT (1, 2) $ zoom _1 $ id .= 3)

   let l = ["a", "ab", "abcdedcba", "zz"]

   (ps, n) <-
      runConduit $ 
         flip execStateT ([], 0) $
            sourceList l $$ countShort' =$ keepPalindroms' =$ sinkNull
   
   print ps
   print n
    
   {-
   flip runStateT ([] :: [Text], 0 :: Int) $
      runConduit $
         sourceList l $$ (zoom _2 countShort) =$ (zoom _1 keepPalindroms) =$ sinkNull
   -}
   return ()


