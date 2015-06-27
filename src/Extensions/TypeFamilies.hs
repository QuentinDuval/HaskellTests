{-# LANGUAGE TypeFamilies, GADTs, RecordWildCards, FlexibleContexts, DataKinds, MultiWayIf, ViewPatterns  #-}

module Extensions.TypeFamilies where

import Control.Monad.Error.Class


-- | Data and interfaces

class IVersionable a where
   type Operation a
   type VersError a
   version  :: MonadError (VersError a) m => a -> Operation a -> m a


class IVersionable a => IRollbackable a where
   type PopVersionError a
   rollback       :: MonadError (PopVersionError a) m => a -> Operation a -> m a
   noVersionError :: a -> PopVersionError a

data IVersionable a => Versionable a = Versionable {
   _object     :: a,
   _version    :: Int,
   _operations :: [Operation a]
}


-- | Public methods

newVersionable :: (IVersionable a) => a -> Versionable a
newVersionable a = Versionable { _object = a, _version = 1, _operations = [] }


addOperation :: (MonadError (VersError a) m, IVersionable a) => Operation a -> Versionable a -> m (Versionable a)
addOperation op Versionable{..} = do
   newObj <- version _object op
   let newOps = op : _operations
   return Versionable { _object = newObj, _version = _version + 1, _operations = newOps }


bulkAddOperation :: (MonadError (VersError a) m, IVersionable a) => Operation a -> [Versionable a] -> m [Versionable a]
bulkAddOperation op = mapM (addOperation op)


popLastOperation :: (MonadError (PopVersionError a) m, IRollbackable a) => Versionable a -> m (Versionable a)
popLastOperation Versionable{..}
   | _version == 1 = throwError $ noVersionError _object -- TODO - Try to avoid this by strong types
   | otherwise = do
      let (lastOp : remOps) = _operations
      newObj <- rollback _object lastOp
      return Versionable { _object = newObj, _version = _version - 1, _operations = remOps }


bulkPopLastOperation :: (MonadError (PopVersionError a) m, IRollbackable a) => [Versionable a] -> m [Versionable a]
bulkPopLastOperation = mapM popLastOperation



