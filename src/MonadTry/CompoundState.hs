module MonadTry.CompoundState where

import Control.Lens.Zoom
import Control.Monad.State



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




