{-# LANGUAGE MultiWayIf, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module MonadTry.CompoundState where

import Control.Lens
import Control.Monad
--import Control.Monad.State                        -- Does not work with MTL StateT...
--import Control.Monad.State.Class (MonadState(..)) -- Does not work with MonadState
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State
import Data.Conduit
import Data.Conduit.List as CL
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

-- TODO - Try with MonadState


-- | CHECK - It might be the same as Control.Lens.Zoom
-- | zoom :: Monad m             => Lens' s t      -> StateT t m a -> StateT s m a
-- | But it does not work for MonadState! => Cannot apply it on a Conduit of stateT

countShort :: (Monad m) => Text -> StateT Int m Text
countShort t = do
   when (T.length t <= 5) $ modify (+1)
   return t

keepPalindroms :: (Monad m) => Text -> StateT [Text] m Text
keepPalindroms t = do
   when (T.reverse t == t) $ modify (t:)
   return t


test :: IO ()
test = do
   let l = ["a", "ab", "abcdedcba", "zz"]

   (ps, n) <-
      runConduit $ 
         flip execStateT ([], 0) $
            sourceList l $$ CL.mapM (zoom _2 . countShort) =$ CL.mapM (zoom _1 . keepPalindroms) =$ sinkNull
   
   print ps
   print n


